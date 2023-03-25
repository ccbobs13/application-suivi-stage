package sn.esmt.applicationsuivistage.repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import sn.esmt.applicationsuivistage.entities.user.Encadreur;

public interface EncadreurRepository extends JpaRepository<Encadreur, Long> {
	List<Encadreur> findByNom(String nom);

	List<Encadreur> findByPrenoms(String prenoms);

	List<Encadreur> findByNomAndPrenoms(String nom, String prenoms);

	List<Encadreur> findByGrade(String grade);
	
	Page<Encadreur> findByEmailContaining(String keyword, Pageable pageable);


	Boolean existsByEmail(String email);
}
