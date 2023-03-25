package sn.esmt.applicationsuivistage.repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import sn.esmt.applicationsuivistage.entities.user.Etudiant;

public interface EtudiantRepository extends JpaRepository<Etudiant, Long> {
	List<Etudiant> findByNom(String nom);

	List<Etudiant> findByPrenoms(String prenoms);

	List<Etudiant> findByNomAndPrenoms(String nom, String prenoms);
	
	Page<Etudiant> findByEmailContaining(String keyword, Pageable pageable);

	Boolean existsByEmail(String email);
}
