package sn.esmt.applicationsuivistage.services;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import sn.esmt.applicationsuivistage.entities.user.Etudiant;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.repositories.EtudiantRepository;

@Service
public class EtudiantService {
	@Autowired
	private EtudiantRepository etudiantRepository;

	@Transactional
	public Etudiant create(Etudiant etudiant) {
		if (Boolean.FALSE.equals(etudiantRepository.existsByEmail(etudiant.getEmail()))) {
			etudiant.setNom(etudiant.getNom().toUpperCase());
			return etudiantRepository.save(etudiant);
		}
		throw new ResourceAlreadyExistsException("Etudiant", "Email", etudiant.getEmail());
	}

	@Transactional(readOnly = true)
	public List<Etudiant> list() {
		return etudiantRepository.findAll();
	}

	@Transactional(readOnly = true)
	public Page<Etudiant> list(int page, int size) {
		return etudiantRepository.findAll(PageRequest.of(page, size));
	}

	@Transactional(readOnly = true)
	public Page<Etudiant> listByEmailContaining(int page, int size, String keyword) {
		return etudiantRepository.findByEmailContaining(keyword, PageRequest.of(page, size));
	}

	@Transactional(readOnly = true)
	public Etudiant find(Long id) {
		return etudiantRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Etudiant", "Id", id));
	}

	@Transactional
	public Etudiant update(Etudiant etudiant, Long id) {

		return etudiantRepository.findById(id).map(e -> {

			if (!Objects.equals(e.getEmail(), etudiant.getEmail())
					&& Boolean.TRUE.equals(etudiantRepository.existsByEmail(etudiant.getEmail()))) {
				throw new ResourceAlreadyExistsException("Etudiant", "Email", etudiant.getEmail());
			}

			e.setEmail(etudiant.getEmail());
			e.setNom(etudiant.getNom().toUpperCase());
			e.setPrenoms(etudiant.getPrenoms());

			if (e.getEncadreur() != etudiant.getEncadreur()) {
				e.setEncadreur(etudiant.getEncadreur());
			}
			if (e.getStage() != etudiant.getStage()) {
				e.setStage(etudiant.getStage());
			}

			return etudiantRepository.save(e);

		}).orElseThrow(() -> new ResourceNotFoundException("Etudiant", "Id", id));
	}

	@Transactional
	public void delete(Long id) {
		Etudiant etudiant = etudiantRepository.findById(id)
				.orElseThrow(() -> new ResourceNotFoundException("Etudiant", "Id", id));
		if (etudiant.getEncadreur() != null) {
			etudiant.getEncadreur().getEtudiants().remove(etudiant);
		}
		if (etudiant.getStage() != null) {
			etudiant.getStage().getEtudiants().remove(etudiant);
		}
		etudiantRepository.delete(etudiant);
	}
}
