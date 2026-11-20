package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.mapper.CharacterMemoryEpisodicMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * 情景记忆关系层服务——记忆内容的唯一事实源；向量表为派生索引，按 uuid 联接。
 * <p>
 * Relational service for episodic memory - the single source of truth for memory
 * content; the vector table is a derived index joined by uuid.
 */
@Slf4j
@Service
public class CharacterMemoryEpisodicService extends ServiceImpl<CharacterMemoryEpisodicMapper, CharacterMemoryEpisodic> {
}
