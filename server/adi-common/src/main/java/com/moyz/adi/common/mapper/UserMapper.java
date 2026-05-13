package com.moyz.adi.common.mapper;

import com.moyz.adi.common.entity.User;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * <p>
 * 用户表 Mapper 接口
 * </p>
 *
 * @author moyz
 * @since 2023-04-11
 */
@Mapper
public interface UserMapper extends BaseMapper<User> {

}
