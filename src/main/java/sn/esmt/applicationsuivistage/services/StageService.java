package sn.esmt.applicationsuivistage.services;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import sn.esmt.applicationsuivistage.entities.Stage;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.repositories.StageRepository;

@Service
public class StageService {
	@Autowired
	private StageRepository stageRepository;

	@Transactional
	public Stage create(Stage stage) {
		if (Boolean.FALSE.equals(stageRepository.existsByTheme(stage.getTheme()))) {
			return stageRepository.save(stage);
		}
		throw new ResourceAlreadyExistsException("Stage", "Theme", stage.getTheme());
	}

	@Transactional(readOnly = true)
	public Stage find(Long id) {
		return stageRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Stage", "Id", id));
	}

	@Transactional(readOnly = true)
	public List<Stage> list() {
		return stageRepository.findAll();
	}

	@Transactional(readOnly = true)
	public Page<Stage> list(int page, int size) {
		return stageRepository.findAll(PageRequest.of(page, size));
	}

	@Transactional(readOnly = true)
	public List<Stage> listByTheme(String theme) {
		return stageRepository.findByTheme(theme);
	}

	@Transactional(readOnly = true)
	public List<Stage> listByThemeContaining(String keyword) {
		return stageRepository.findByThemeContaining(keyword);
	}

	@Transactional(readOnly = true)
	public Page<Stage> listByThemeContaining(int page, int size, String keyword) {
		return stageRepository.findByThemeContaining(keyword, PageRequest.of(page, size));
	}

	@Transactional
	public Stage update(Stage stage, Long id) {
		return stageRepository.findById(id).map(e -> {
			if (!Objects.equals(stage.getTheme(), e.getTheme()) && Boolean.TRUE.equals(stageRepository.existsByTheme(stage.getTheme()))) {
				throw new ResourceAlreadyExistsException("Stage", "Theme", stage.getTheme());
			}
			e.setTheme(stage.getTheme());
			return stageRepository.save(e);
		}).orElseThrow(() -> new ResourceNotFoundException("Stage", "Id", id));
	}

	@Transactional
	public void delete(Long id) {
		Stage stage = stageRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Stage", "Id", id));
		stageRepository.delete(stage);
	}
}
