package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.enums.UserStatusEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.AdiMailSender;
import com.moyz.adi.common.mapper.UserMapper;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.CostStat;
import com.moyz.adi.common.vo.TokenCostStatistic;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.mindrot.jbcrypt.BCrypt;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.*;
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * <p>
 * User service implementation class
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

    @Resource
    private AdiProperties adiProperties;

    @Value("${spring.application.name}")
    private String appName;

    /**
     * 通过邮箱获取用户|Get user by email
     *
     * @param email 邮箱|email
     * @return 用户|user
     */
    public User getByEmail(String email) {
        if (StringUtils.isBlank(email)) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        return this.lambdaQuery().eq(User::getEmail, email).eq(User::getIsDeleted, false).oneOpt().orElseThrow(() -> new BaseException(A_USER_NOT_EXIST));
    }

    /**
     * 通过邮箱找回密码|Forgot password by email
     *
     * @param email 邮箱|email
     */
    public void forgotPassword(String email) {
        User user = getByEmail(email);
        String code = UuidUtil.createShort();
        String key = MessageFormat.format(FIND_MY_PASSWORD, code);
        stringRedisTemplate.opsForValue().set(key, user.getId().toString(), 8, TimeUnit.HOURS);
        adiMailSender.send(appName + "重置密码", "点击链接将密码重置为" + AdiConstant.DEFAULT_PASSWORD + "，链接(" + AdiConstant.AUTH_ACTIVE_CODE_EXPIRE + "小时内有效):" + adiProperties.getBackendUrl() + "/auth/password/reset?code=" + code, email);
    }

    /**
     * 注册|Register
     *
     * @param email     邮箱|email
     * @param password  密码|password
     * @param captchaId 验证码ID|captcha ID
     * @param captcha   验证码|captcha
     */
    public void register(String email, String password, String captchaId, String captcha) {
        //验证码
        String captchaIdKey = MessageFormat.format(AUTH_REGISTER_CAPTCHA_ID, captchaId);
        String captchaInCache = stringRedisTemplate.opsForValue().get(captchaIdKey);
        if (StringUtils.isBlank(captchaInCache) || !captchaInCache.equalsIgnoreCase(captcha)) {
            throw new BaseException(A_LOGIN_CAPTCHA_ERROR);
        }
        stringRedisTemplate.delete(captchaInCache);

        User user = ChainWrappers.lambdaQueryChain(baseMapper).eq(User::getIsDeleted, false).eq(User::getEmail, email).one();
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
        newOne.setUuid(UuidUtil.createShort());
        newOne.setEmail(email);
        newOne.setPassword(hashed);
        newOne.setUserStatus(UserStatusEnum.WAIT_CONFIRM);
        baseMapper.insert(newOne);

        //Create default conversation
        conversationService.createDefault(newOne.getId());
    }

    /**
     * 重置密码|Reset password
     *
     * @param code 重置密码code|reset password code
     */
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

    /**
     * 修改密码|Modify password
     *
     * @param oldPassword 旧密码|old password
     * @param newPassword 新密码|new password
     */
    public void modifyPassword(String oldPassword, String newPassword) {
        User user = ThreadContext.getExistCurrentUser();

        if (!BCrypt.checkpw(oldPassword, user.getPassword())) {
            throw new BaseException(A_OLD_PASSWORD_INVALID);
        }

        String hashed = BCrypt.hashpw(newPassword, BCrypt.gensalt());
        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setPassword(hashed);
        baseMapper.updateById(updateUser);
    }

    /**
     * 激活|Activate
     *
     * @param activeCode 激活码|activation code
     */
    public void active(String activeCode) {
        String activeCodeKey = MessageFormat.format(AUTH_ACTIVE_CODE, activeCode);
        String email = stringRedisTemplate.opsForValue().get(activeCodeKey);
        if (StringUtils.isBlank(email)) {
            throw new BaseException(A_ACTIVE_CODE_INVALID);
        }

        User user = this.lambdaQuery().eq(User::getEmail, email).eq(User::getIsDeleted, false).oneOpt().orElse(null);
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }

        stringRedisTemplate.delete(activeCodeKey);

        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setUserStatus(UserStatusEnum.NORMAL);
        updateUser.setActiveTime(LocalDateTime.now());
        baseMapper.updateById(updateUser);

        setLoginToken(user);
    }

    /**
     * 通过UUID激活|Activate by UUID
     *
     * @param uuid 用户UUID|user UUID
     */
    public void activeByUuid(String uuid) {
        User user = this.getByUuidOrThrow(uuid);
        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setUserStatus(UserStatusEnum.NORMAL);
        updateUser.setActiveTime(LocalDateTime.now());
        baseMapper.updateById(updateUser);
    }

    /**
     * 冻结用户|Freeze user
     *
     * @param uuid 用户UUID|user UUID
     */
    public void freeze(String uuid) {
        User user = this.getByUuidOrThrow(uuid);
        User updateUser = new User();
        updateUser.setId(user.getId());
        updateUser.setUserStatus(UserStatusEnum.FREEZE);
        baseMapper.updateById(updateUser);
    }

    /**
     * 编辑用户|Edit user
     *
     * @param userEditReq 用户编辑请求|user edit request
     */
    public void editUser(UserEditReq userEditReq) {
        User user = this.getByUuidOrThrow(userEditReq.getUuid());
        User editUser = new User();
        editUser.setId(user.getId());
        BeanUtils.copyProperties(userEditReq, editUser);
        if (StringUtils.isNotBlank(userEditReq.getPassword())) {
            String hashed = BCrypt.hashpw(userEditReq.getPassword(), BCrypt.gensalt());
            editUser.setPassword(hashed);
        } else {
            editUser.setPassword(null);
        }
        baseMapper.updateById(editUser);
    }

    /**
     * 登录|Login
     *
     * @param loginReq 登录请求|login request
     * @return 登录响应|login response
     */
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

        User user = this.lambdaQuery().eq(User::getIsDeleted, false).eq(User::getEmail, loginReq.getEmail()).oneOpt().orElseThrow(() -> new BaseException(ErrorEnum.A_USER_NOT_EXIST));
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

    /**
     * 设置并获取登录验证码ID|Set and get login captcha ID
     *
     * @return 登录验证码ID|login captcha ID
     */
    public String setAndGetLoginCaptchaId() {
        String captchaId = UuidUtil.createShort();
        String captchaIdKey = MessageFormat.format(AUTH_LOGIN_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captchaId, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
        return captchaId;
    }

    /**
     * 缓存登录验证码|Cache login captcha
     *
     * @param captchaId 验证码ID|captcha ID
     * @param captcha   验证码|captcha
     */
    public void cacheLoginCaptcha(String captchaId, String captcha) {
        String captchaIdKey = MessageFormat.format(AUTH_LOGIN_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captcha, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
    }

    /**
     * 缓存注册验证码|Cache register captcha
     *
     * @param captchaId 验证码ID|captcha ID
     * @param captcha   验证码|captcha
     */
    public void cacheRegisterCaptcha(String captchaId, String captcha) {
        String captchaIdKey = MessageFormat.format(AUTH_REGISTER_CAPTCHA_ID, captchaId);
        stringRedisTemplate.opsForValue().set(captchaIdKey, captcha, AdiConstant.AUTH_CAPTCHA_ID_EXPIRE, TimeUnit.HOURS);
    }

    /**
     * 获取配置|Get config
     *
     * @return 配置响应|config response
     */
    public ConfigResp getConfig() {
        ConfigResp result = new ConfigResp();
        User user = ThreadContext.getCurrentUser();

        result.setContextMsgPairNum(user.getUnderstandContextMsgPairNum());
        //User quota
        result.setUserQuota(UserQuota.builder().requestTimesByDay(user.getQuotaByRequestDaily()).requestTimesByMonth(user.getQuotaByRequestMonthly()).drawByDay(user.getQuotaByImageDaily()).drawByMonth(user.getQuotaByImageMonthly()).tokenByDay(user.getQuotaByTokenDaily()).tokenByMonth(user.getQuotaByTokenMonthly()).build());
        //User cost
        CostStatResp quotaCostResp = new CostStatResp();
        setPaidCostStat(user, quotaCostResp);
        setFreeCostStat(user, quotaCostResp);
        result.setQuotaCost(quotaCostResp);
        return result;
    }

    /**
     * 设置付费费用统计|Set paid cost statistics
     *
     * @param user          用户|user
     * @param quotaCostResp 配额费用响应|quota cost response
     */
    private void setPaidCostStat(User user, CostStatResp quotaCostResp) {
        CostStat cost = userDayCostService.costStatByUser(user.getId(), false);
        quotaCostResp.setPaidTokenCost(TokenCostStatistic.builder().todayTokenCost(cost.getTextTokenCostByDay()).monthTokenCost(cost.getTextTokenCostByMonth()).build());
        quotaCostResp.setPaidRequestTimes(RequestTimesStatistic.builder().todayRequestTimes(cost.getTextRequestTimesByDay()).monthRequestTimes(cost.getTextRequestTimesByMonth()).build());
        quotaCostResp.setPaidDrawTimes(DrawTimesStatistic.builder().todayDrawTimes(cost.getDrawTimesByDay()).monthDrawTimes(cost.getDrawTimesByMonth()).build());
    }

    /**
     * 设置免费费用统计|Set free cost statistics
     *
     * @param user          用户|user
     * @param quotaCostResp 配额费用响应|quota cost response
     */
    private void setFreeCostStat(@NotNull User user, CostStatResp quotaCostResp) {
        CostStat cost = userDayCostService.costStatByUser(user.getId(), true);
        quotaCostResp.setFreeTokenCost(TokenCostStatistic.builder().todayTokenCost(cost.getTextTokenCostByDay()).monthTokenCost(cost.getTextTokenCostByMonth()).build());
        quotaCostResp.setFreeRequestTimes(RequestTimesStatistic.builder().todayRequestTimes(cost.getTextRequestTimesByDay()).monthRequestTimes(cost.getTextRequestTimesByMonth()).build());
        quotaCostResp.setFreeDrawTimes(DrawTimesStatistic.builder().todayDrawTimes(cost.getDrawTimesByDay()).monthDrawTimes(cost.getDrawTimesByMonth()).build());
    }

    /**
     * 更新配置|Update config
     *
     * @param userUpdateReq 用户更新请求|user update request
     */
    public void updateConfig(UserUpdateReq userUpdateReq) {
        User user = new User();
        user.setId(ThreadContext.getCurrentUserId());
        BeanUtils.copyProperties(userUpdateReq, user);
        baseMapper.updateById(user);
    }

    /**
     * 注销|Logout
     */
    public void logout() {
        String token = ThreadContext.getToken();
        if (null == token) {
            log.warn("logout token is null");
            return;
        }
        String tokenKey = MessageFormat.format(USER_TOKEN, token);
        stringRedisTemplate.delete(tokenKey);
    }

    /**
     * 设置用户的登录令牌|Set the login token for the user
     *
     * @param user 要设置登录令牌的用户|the user for whom the login token is being set
     * @return 生成的登录令牌|the generated login token
     */
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
        String token = UuidUtil.createShort();
        String tokenKey = MessageFormat.format(USER_TOKEN, token);
        String jsonUser = JsonUtil.toJson(user);
        log.info("jsonUser:{}", jsonUser);
        stringRedisTemplate.opsForValue().set(tokenKey, jsonUser, AdiConstant.USER_TOKEN_EXPIRE, TimeUnit.HOURS);
        return token;
    }

    /**
     * 发送激活链接|Send activation email
     *
     * @param email 用户邮箱|user email
     */
    public void sendActiveEmail(String email) {
        String activeCode = UuidUtil.createShort();
        String activeCodeKey = MessageFormat.format(AUTH_ACTIVE_CODE, activeCode);
        stringRedisTemplate.opsForValue().set(activeCodeKey, email, AdiConstant.AUTH_ACTIVE_CODE_EXPIRE, TimeUnit.HOURS);
        adiMailSender.send("欢迎注册AIDeepIn", "激活链接(" + AdiConstant.AUTH_ACTIVE_CODE_EXPIRE + "小时内有效):" + adiProperties.getBackendUrl() + "/auth/active?code=" + activeCode, email);
    }

    /**
     * 通过用户ID获取用户|Get user by user ID
     *
     * @param id 用户ID|user ID
     * @return 用户|user
     */
    @Cacheable(cacheNames = USER_INFO, condition = "#id>0", key = "#p0")
    public User getByUserId(Long id) {
        return ChainWrappers.lambdaQueryChain(baseMapper).eq(User::getId, id).one();
    }

    /**
     * 通过UUID获取用户|Get user by UUID
     *
     * @param uuid 用户UUID|user UUID
     * @return 用户|user
     */
    public User getByUuid(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper).eq(User::getUuid, uuid).one();
    }

    /**
     * 通过UUID获取用户，如果不存在则抛出异常|Get user by UUID or throw exception if not exist
     *
     * @param uuid 用户UUID|user UUID
     * @return 用户|user
     */
    public User getByUuidOrThrow(String uuid) {
        User user = this.getByUuid(uuid);
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        return user;
    }

    /**
     * 搜索用户|Search users
     *
     * @param req         用户搜索请求|user search request
     * @param currentPage 当前页码|current page number
     * @param pageSize    每页大小|page size
     * @return 用户信息分页|page of user information
     */
    public Page<UserInfoDto> search(UserSearchReq req, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<User> wrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(req.getName())) {
            wrapper.like(User::getName, req.getName());
        }
        if (StringUtils.isNotBlank(req.getUuid())) {
            wrapper.eq(User::getUuid, req.getUuid());
        }
        if (StringUtils.isNotBlank(req.getEmail())) {
            wrapper.eq(User::getEmail, req.getEmail());
        }
        if (null != req.getUserStatus()) {
            wrapper.eq(User::getUserStatus, UserStatusEnum.getByValue(req.getUserStatus()));
        }
        if (null != req.getCreateTime() && req.getCreateTime().length == 2) {
            wrapper.between(User::getCreateTime, LocalDateTimeUtil.parse(req.getCreateTime()[0]), LocalDateTimeUtil.parse(req.getCreateTime()[1]));
        }
        if (null != req.getUpdateTime() && req.getUpdateTime().length == 2) {
            wrapper.between(User::getUpdateTime, LocalDateTimeUtil.parse(req.getUpdateTime()[0]), LocalDateTimeUtil.parse(req.getUpdateTime()[1]));
        }
        if (null != req.getIsAdmin()) {
            wrapper.eq(User::getIsAdmin, req.getIsAdmin());
        }
        wrapper.eq(User::getIsDeleted, false);
        wrapper.orderByDesc(User::getUpdateTime);
        Page<User> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);
        Page<UserInfoDto> result = new Page<>();
        return MPPageUtil.convertToPage(page, result, UserInfoDto.class);
    }

    /**
     * 添加用户|Add user
     *
     * @param addUserReq 添加用户请求|add user request
     * @return 用户信息|user information
     */
    public UserInfoDto addUser(UserAddReq addUserReq) {
        User user = this.lambdaQuery().eq(User::getIsDeleted, false).eq(User::getEmail, addUserReq.getEmail()).one();
        if (null != user) {
            throw new BaseException(A_USER_EXIST);
        }

        String hashed = BCrypt.hashpw(addUserReq.getPassword(), BCrypt.gensalt());
        String uuid = UuidUtil.createShort();
        User newOne = new User();
        if (StringUtils.isNotBlank(addUserReq.getName())) {
            newOne.setName(addUserReq.getName());
        } else {
            newOne.setName(StringUtils.substringBefore(addUserReq.getEmail(), "@"));
        }
        newOne.setUuid(uuid);
        newOne.setEmail(addUserReq.getEmail());
        newOne.setPassword(hashed);
        newOne.setUserStatus(UserStatusEnum.NORMAL);
        newOne.setActiveTime(LocalDateTime.now());
        baseMapper.insert(newOne);

        UserInfoDto result = new UserInfoDto();
        User newUser = this.getByUuid(uuid);
        BeanUtils.copyProperties(newUser, result);
        return result;
    }
}
