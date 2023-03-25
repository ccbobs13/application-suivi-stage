package sn.esmt.applicationsuivistage.services;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import sn.esmt.applicationsuivistage.entities.user.Encadreur;
import sn.esmt.applicationsuivistage.entities.user.Etudiant;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.repositories.EncadreurRepository;
import sn.esmt.applicationsuivistage.repositories.EtudiantRepository;

@Service
public class EncadreurService {
	@Autowired
	private EncadreurRepository encadreurRepository;

	@Autowired
	private EtudiantRepository etudiantRepository;

	@Transactional
	public Encadreur create(Encadreur encadreur) {
		if (Boolean.FALSE.equals(encadreurRepository.existsByEmail(encadreur.getEmail()))) {
			encadreur.setNom(encadreur.getEmail().toUpperCase());
			return encadreurRepository.save(encadreur);
		}
		throw new ResourceAlreadyExistsException("Encadreur", "Email", encadreur.getEmail());
	}

	@Transactional(readOnly = true)
	public List<Encadreur> list() {
		return encadreurRepository.findAll();
	}

	@Transactional(readOnly = true)
	public Page<Encadreur> list(int page, int size) {
		return encadreurRepository.findAll(PageRequest.of(page, size));
	}

	@Transactional(readOnly = true)
	public Page<Encadreur> listByEmailContaining(int page, int size, String keyword) {
		return encadreurRepository.findByEmailContaining(keyword, PageRequest.of(page, size));
	}

	@Transactional(readOnly = true)
	public Encadreur find(Long id) {
		return encadreurRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Encadreur", "Id", id));
	}

	@Transactional
	public Encadreur update(Encadreur encadreur, Long id) {
		return encadreurRepository.findById(id).map(e -> {

			if (!Objects.equals(e.getEmail(), encadreur.getEmail())
					&& Boolean.TRUE.equals(encadreurRepository.existsByEmail(encadreur.getEmail()))) {
				throw new ResourceAlreadyExistsException("Encadreur", "Email", encadreur.getEmail());
			}

			e.setEmail(encadreur.getEmail());
			e.setNom(encadreur.getNom().toUpperCase());
			e.setPrenoms(encadreur.getPrenoms());
			e.setGrade(encadreur.getGrade());
			return encadreurRepository.save(e);
		}).orElseThrow(() -> new ResourceNotFoundException("Encadreur", "Id", id));
	}

	@Transactional
	public void delete(Long id) {
		Encadreur encadreur = encadreurRepository.findById(id)
				.orElseThrow(() -> new ResourceNotFoundException("Encadreur", "Id", id));
		encadreur.getEtudiants().forEach(etudiant -> etudiant.setEncadreur(null));
		encadreurRepository.delete(encadreur);
	}

	@Transactional
	public void removeStudent(Long id, Long idEtu) {
		Encadreur encadreur = encadreurRepository.findById(id)
				.orElseThrow(() -> new ResourceNotFoundException("Encadreur", "Id", id));
		Etudiant etudiant = etudiantRepository.findById(idEtu)
				.orElseThrow(() -> new ResourceNotFoundException("Etudiant", "Id", id));

		if (encadreur.getEtudiants().contains(etudiant)) {
			encadreur.getEtudiants().remove(etudiant);
			etudiant.setEncadreur(null);
			encadreurRepository.save(encadreur);
		} else {
			throw new IllegalArgumentException("Cet étudiant n'est pas supervisé par cet encadreur");
		}

	}
}
