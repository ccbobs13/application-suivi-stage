package sn.esmt.applicationsuivistage.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import sn.esmt.applicationsuivistage.entities.Role;

public interface RoleRepository extends JpaRepository<Role, Long> {
	Optional<Role> findByNom(String nom);

	Boolean existsByNom(String nom);
}
