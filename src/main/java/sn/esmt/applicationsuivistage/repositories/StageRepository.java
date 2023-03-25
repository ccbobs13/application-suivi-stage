package sn.esmt.applicationsuivistage.repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import sn.esmt.applicationsuivistage.entities.Stage;

public interface StageRepository extends JpaRepository<Stage, Long> {
	List<Stage> findByTheme(String theme);

	List<Stage> findByThemeContaining(String keyword);

	Page<Stage> findByThemeContaining(String keyword, Pageable pageable);

	Boolean existsByTheme(String theme);
}
