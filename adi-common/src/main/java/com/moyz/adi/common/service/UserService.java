package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.ConfigResp;
import com.moyz.adi.common.dto.LoginReq;
import com.moyz.adi.common.dto.LoginResp;
import com.moyz.adi.common.dto.UserUpdateReq;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.enums.UserStatusEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.AdiMailSender;
import com.moyz.adi.common.mapper.UserMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.CostStat;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.mindrot.jbcrypt.BCrypt;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.text.MessageFormat;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.*;
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * <p>
 * 用户表 服务实现类
 * </p>
 *
 * @author moyz
 * @since 2023-04-11
 */
@Slf4j
@Service
public class UserService extends ServiceImpl<UserMapper, User> {

    @Resource
    private UserDayCostService userDayCostService;

    @Resource
    private AdiMailSender adiMailSender;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private ConversationService conversationService;

    @Value("${adi.backend-url}")
    private String backendUrl;

    @Value("${spring.application.name}")
    private String appName;

    public User getByEmail(String email) {
        if (StringUtils.isBlank(email)) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        return this.lambdaQuery()
                .eq(User::getEmail, email)
                .eq(User::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_USER_NOT_EXIST));
    }

    public void forgotPassword(String email) {
        User user = getByEmail(email);
        String code = UUID.randomUUID().toString().replace("-", "");
        String key = MessageFormat.format(FIND_MY_PASSWORD, code);
        stringRedisTemplate.opsForValue().set(key, user.getId().toString(), 8, TimeUnit.HOURS);
        adiMailSender.send(appName + "重置密码", "点击链接将密码重置为" + AdiConstant.DEFAULT_PASSWORD + "，链接(" + AdiConstant.AUTH_ACTIVE_CODE_EXPIRE + "小时内有效):" + backendUrl + "/auth/password/reset?code=" + code, email);
    }

    public void register(String email, String password, String captchaId, String captcha) {
        //验证码
        String captchaIdKey = MessageFormat.format(AUTH_REGISTER_CAPTCHA_ID, captchaId);
        String captchaInCache = stringRedisTemplate.opsForValue().get(captchaIdKey);
        if (StringUtils.isBlank(captchaInCache) || !captchaInCache.equalsIgnoreCase(captcha)) {
            throw new BaseException(A_LOGIN_CAPTCHA_ERROR);
        }
        stringRedisTemplate.delete(captchaInCache);

        User user = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(User::getIsDeleted, false)
                .eq(User::getEmail, email)
                .one();
        if (null != user && user.getUserStatus() == UserStatusEnum.NORMAL) {
            throw new BaseException(A_USER_EXIST);
        }
        if (null != user) {
            sendActiveEmail(email);
            return;
        }

        //发送激活链接
        sendActiveEmail(email);

        String hashed = BCrypt.hashpw(password, BCrypt.gensalt());

        //创建用户
        User newOne = new User();
        newOne.setName(StringUtils.substringBefore(email, "@"));
        newOne.setUuid(UUID.randomUUID().toString().replace("-", ""));
        newOne.setEmail(email);
        newOne.setPassword(hashed);
        newOne.setUserStatus(UserStatusEnum.WAIT_CONFIRM);
        baseMapper.insert(newOne);
    }

    public void resetPassword(String code) {
        String key = MessageFormat.format(FIND_MY_PASSWORD, code);
        String userId = stringRedisTemplate.opsForValue().get(key);
        if (StringUtils.isBlank(userId)) {
            throw new BaseException(A_FIND_PASSWORD_CODE_ERROR);
        }
        User updateUser = new User();
        updateUser.setId(Long.parseLong(userId));
        updateUser.setPassword(BCrypt.hashpw(AdiConstant.DEFAULT_PASSWORD, BCrypt.gensalt()));
        baseMapper.updateById(updateUser);
        stringRedisTemplate.delete(key);
    }

    public void modifyPassword(String oldPassword, String newPassword) {
        User user = ThreadContext.getExistCurrentUser();

        if (!BCrypt.checkpw(oldPassword, user.getPassword())) {
            throw new RuntimeException("原密码不正确");
        }

        String hashed = BCrypt.hashpw(newPassword, BCrypt.gensalt());
        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setPassword(hashed);
        baseMapper.updateById(updateUser);
    }

    public void active(String activeCode) {
        String activeCodeKey = MessageFormat.format(AUTH_ACTIVE_CODE, activeCode);
        String email = stringRedisTemplate.opsForValue().get(activeCodeKey);
        if (StringUtils.isBlank(email)) {
            throw new RuntimeException("激活码已失效");
        }

        LambdaQueryWrapper<User> queryWrapper = new LambdaQueryWrapper<>();
        User user = this.lambdaQuery()
                .eq(User::getEmail, email)
                .eq(User::getIsDeleted, false)
                .oneOpt()
                .orElse(null);
        if (null == user) {
            throw new RuntimeException("用户不存在");
        }

        stringRedisTemplate.delete(activeCodeKey);

        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setUserStatus(UserStatusEnum.NORMAL);
        baseMapper.updateById(updateUser);

        setLoginToken(user);

        //Create default conversation
        conversationService.createDefault(user.getId());
    }

    public LoginResp login(LoginReq loginReq) {
        //captcha check
        String failCountKey = MessageFormat.format(RedisKeyConstant.LOGIN_FAIL_COUNT, loginReq.getEmail());
        int passwordFailCount = 0;
        String failCountVal = stringRedisTemplate.opsForValue().get(failCountKey);
        if (StringUtils.isNotBlank(failCountVal)) {
            passwordFailCount = Integer.parseInt(failCountVal);
        }
        if (passwordFailCount >= AdiConstant.LOGIN_MAX_FAIL_TIMES) {
            if (StringUtils.isAnyBlank(loginReq.getCaptchaCode(), loginReq.getCaptchaId())) {
                String captchaId = setAndGetLoginCaptchaId();
                LoginResp loginResp = new LoginResp();
                loginResp.setCaptchaId(captchaId);
                throw new BaseException(ErrorEnum.A_LOGIN_ERROR_MAX).setData(loginResp);
            }
            String captchaIdKey = MessageFormat.format(AUTH_LOGIN_CAPTCHA_ID, loginReq.getCaptchaId());
            String captcha = stringRedisTemplate.opsForValue().get(captchaIdKey);
            if (StringUtils.isBlank(captcha) || !captcha.equalsIgnoreCase(loginReq.getCaptchaCode())) {
                throw new BaseException(A_LOGIN_CAPTCHA_ERROR);
            }
        }
        //captcha check end

        User user = this.lambdaQuery()
                .eq(User::getIsDeleted, false)
                .eq(User::getEmail, loginReq.getEmail())
                .oneOpt()
                .orElseThrow(() -> new BaseException(ErrorEnum.A_USER_NOT_EXIST));
        if (user.getUserStatus() == UserStatusEnum.WAIT_CONFIRM) {
            throw new BaseException(ErrorEnum.A_USER_WAIT_CONFIRM);
        }
        if (!BCrypt.checkpw(loginReq.getPassword(), user.getPassword())) {

            //计算错误次数并判断下次登录是否要输入验证码
            passwordFailCount = passwordFailCount + 1;
            stringRedisTemplate.opsForValue().set(failCountKey, String.valueOf(passwordFailCount), AdiConstant.USER_TOKEN_EXPIRE, TimeUnit.HOURS);

            throw new BaseException(ErrorEnum.A_LOGIN_ERROR);
        }

        //login success
        stringRedisTemplate.delete(failCountKey);
        String token = setLoginToken(user);
        LoginResp loginResp = new LoginResp();
        loginResp.setToken(token);
        BeanUtils.copyProperties(user, loginResp);
        return loginResp;
    }

    public String setAndGetLoginCaptchaId() {
        String captchaId = UUID.randomUUID().toString().replace("-", "");
        String captchaIdKey = MessageFormat.format(AUTH_LOGIN_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captchaId, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
        return captchaId;
    }

    public void cacheLoginCaptcha(String captchaId, String captcha) {
        String captchaIdKey = MessageFormat.format(AUTH_LOGIN_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captcha, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
    }

    public void cacheRegisterCaptcha(String captchaId, String captcha) {
        String captchaIdKey = MessageFormat.format(AUTH_REGISTER_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captcha, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
    }

    public ConfigResp getConfig() {
        ConfigResp result = new ConfigResp();
        User user = ThreadContext.getCurrentUser();
        BeanUtils.copyProperties(user, result);

        CostStat costStat = userDayCostService.costStatByUser(user.getId());
        result.setTodayTokenCost(costStat.getTextTokenCostByDay());
        result.setTodayRequestTimes(costStat.getTextRequestTimesByDay());
        result.setTodayGeneratedImageNumber(costStat.getImageGeneratedNumberByDay());
        result.setCurrMonthTokenCost(costStat.getTextTokenCostByMonth());
        result.setCurrMonthRequestTimes(costStat.getTextRequestTimesByMonth());
        result.setCurrMonthGeneratedImageNumber(costStat.getImageGeneratedNumberByMonth());
        return result;
    }

    public void updateConfig(UserUpdateReq userUpdateReq) {
        User user = new User();
        user.setId(ThreadContext.getCurrentUserId());
        BeanUtils.copyProperties(userUpdateReq, user);
        baseMapper.updateById(user);
    }

    public void logout() {
        String token = ThreadContext.getToken();
        if (null == token) {
            log.warn("logout token is null");
            return;
        }
        String tokenKey = MessageFormat.format(USER_TOKEN, token);
        stringRedisTemplate.delete(tokenKey);
    }

    private String setLoginToken(User user) {
        if (user.getQuotaByTokenDaily() == 0) {
            user.setQuotaByTokenDaily(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_TOKEN_DAILY)));
        }
        if (user.getQuotaByTokenMonthly() == 0) {
            user.setQuotaByTokenMonthly(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_TOKEN_MONTHLY)));
        }
        if (user.getQuotaByRequestDaily() == 0) {
            user.setQuotaByRequestDaily(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_REQUEST_DAILY)));
        }
        if (user.getQuotaByRequestMonthly() == 0) {
            user.setQuotaByRequestMonthly(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_REQUEST_MONTHLY)));
        }
        if (user.getQuotaByImageDaily() == 0) {
            user.setQuotaByImageDaily(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_IMAGE_DAILY)));
        }
        if (user.getQuotaByImageMonthly() == 0) {
            user.setQuotaByImageMonthly(Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.QUOTA_BY_IMAGE_MONTHLY)));
        }
        String token = UUID.randomUUID().toString().replace("-", "");
        String tokenKey = MessageFormat.format(USER_TOKEN, token);
        String jsonUser = JsonUtil.toJson(user);
        log.info("jsonUser:{}", jsonUser);
        stringRedisTemplate.opsForValue().set(tokenKey, jsonUser, AdiConstant.USER_TOKEN_EXPIRE, TimeUnit.HOURS);
        return token;
    }

    /**
     * 发送激活链接
     *
     * @param email
     */
    public void sendActiveEmail(String email) {
        String activeCode = UUID.randomUUID().toString().replace("-", "");
        String activeCodeKey = MessageFormat.format(AUTH_ACTIVE_CODE, activeCode);
        stringRedisTemplate.opsForValue().set(activeCodeKey, email, AdiConstant.AUTH_ACTIVE_CODE_EXPIRE, TimeUnit.HOURS);
        adiMailSender.send("欢迎注册AIDeepIn", "激活链接(" + AdiConstant.AUTH_ACTIVE_CODE_EXPIRE + "小时内有效):" + backendUrl + "/auth/active?code=" + activeCode, email);
    }

    public User getByUuid(String uuid){
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(User::getUuid, uuid)
                .one();
    }

}
