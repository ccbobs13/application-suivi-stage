package sn.esmt.applicationsuivistage.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import sn.esmt.applicationsuivistage.entities.user.User;

public interface UserRepository extends JpaRepository<User, Long> {
	Optional<User> findByEmail(String email);

	Boolean existsByEmail(String email);

	User findByEmailAndPassword(String email, String password);
}
