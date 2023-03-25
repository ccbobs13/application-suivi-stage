package sn.esmt.applicationsuivistage.services;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import sn.esmt.applicationsuivistage.entities.Role;
import sn.esmt.applicationsuivistage.entities.user.User;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.repositories.RoleRepository;
import sn.esmt.applicationsuivistage.repositories.UserRepository;

@Service
public class UserService {

	@Autowired
	private RoleRepository roleRepository;

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private PasswordEncoder passwordEncoder;

	@Transactional
	public User create(User user) {
		if (Boolean.FALSE.equals(userRepository.existsByEmail(user.getEmail()))) {
			user.setPassword(passwordEncoder.encode(user.getPassword()));
			return userRepository.save(user);
		}
		throw new ResourceAlreadyExistsException("User", "Email", user.getEmail());
	}

	@Transactional
	public User create(User user, String roleName) {
		if (Boolean.FALSE.equals(userRepository.existsByEmail(user.getEmail()))) {
			Role role = roleRepository.findByNom(roleName)
					.orElseThrow(() -> new ResourceNotFoundException("Role", "Nom", roleName));
			user.setPassword(passwordEncoder.encode(user.getPassword()));
			user.setRoles(Arrays.asList(role));
			return userRepository.save(user);
		}
		throw new ResourceAlreadyExistsException("User", "Email", user.getEmail());
	}

	@Transactional(readOnly = true)
	public List<User> list() {
		return userRepository.findAll();
	}

	@Transactional
	public User update(User user, Long id) {
		User existingUser = userRepository.findById(id)
				.orElseThrow(() -> new ResourceNotFoundException("User", "Id", id));
		User existingUserByEmail = userRepository.findByEmail(user.getEmail())
				.orElseThrow(() -> new ResourceNotFoundException("User", "Email", user.getEmail()));

		if (existingUserByEmail != null && !existingUserByEmail.getId().equals(existingUser.getId())) {
			throw new ResourceAlreadyExistsException("User", "email", user.getEmail());
		}

//		existingUser.setNom(user.getNom());
//		existingUser.setPrenoms(user.getPrenoms());
		existingUser.setEmail(user.getEmail());
		return userRepository.save(existingUser);

	}

	@Transactional
	public User addRole(Long id, String roleName) throws ResourceNotFoundException, IllegalArgumentException {
		User user = userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User", "Id", id));
		Role role = roleRepository.findByNom(roleName)
				.orElseThrow(() -> new ResourceNotFoundException("Role", "Nom", id));
		if (role != null) {
			user.setRoles(Arrays.asList(role));
			return userRepository.save(user);
		} else {
			throw new IllegalArgumentException("Role " + roleName + "is not valid");
		}
	}

	@Transactional
	public void addRoles(Long id, List<Role> roles) {
		// TODO implement
	}

	@Transactional
	public User removeRole(Long id, String roleName) {
		User user = userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User", "Id", id));
		Role role = roleRepository.findByNom(roleName)
				.orElseThrow(() -> new ResourceNotFoundException("Role", "Nom", id));
		if (role != null) {
			user.getRoles().remove(role);
			return userRepository.save(user);
		} else {
			throw new IllegalArgumentException("Role " + roleName + "is not valid");
		}
	}

	@Transactional
	public void delete(Long id) {
		User user = userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User", "Id", id));
		user.getRoles().forEach(r -> r.getUsers().remove(user));
		userRepository.delete(user);
	}

}
