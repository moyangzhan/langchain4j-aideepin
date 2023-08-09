package com.moyz.adi.common.mapper;

import com.moyz.adi.common.entity.Conversation;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * <p>
 * 会话表 Mapper 接口
 * </p>
 *
 * @author moyz
 * @since 2023-04-11
 */
@Mapper
public interface ConversationMapper extends BaseMapper<Conversation> {

}
