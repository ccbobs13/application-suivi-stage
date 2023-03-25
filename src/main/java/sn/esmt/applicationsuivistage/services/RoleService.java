package sn.esmt.applicationsuivistage.services;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import sn.esmt.applicationsuivistage.entities.Role;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.repositories.RoleRepository;

@Service
public class RoleService {

	@Autowired
	private RoleRepository roleRepository;

	@Transactional
	public Role create(String nom) {
		if (Boolean.FALSE.equals(roleRepository.existsByNom(nom))) {
			Role role = new Role();
			role.setNom(nom);
			return roleRepository.save(role);
		}
		throw new ResourceAlreadyExistsException("Role", "Nom", nom);
	}

	@Transactional(readOnly = true)
	public List<Role> list() {
		return roleRepository.findAll();
	}

	@Transactional(readOnly = true)
	public Role find(Long id) {
		return roleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Role", "Id", id));
	}

	@Transactional(readOnly = true)
	public Role find(String nom) {
		return roleRepository.findByNom(nom).orElseThrow(() -> new ResourceNotFoundException("Role", "Nom", nom));
	}

	@Transactional
	public Role update(Long id, String nom) {
		Role role = roleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Role", "Id", id));
		if (!Objects.equals(role.getNom(), nom) && Boolean.TRUE.equals(roleRepository.existsByNom(nom))) {
			throw new ResourceAlreadyExistsException("Role", "Nom", nom);
		}
		role.setNom(nom);
		return roleRepository.save(role);
	}

	@Transactional
	public void delete(Long id) {
		Role role = roleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Role", "Id", id));
		roleRepository.delete(role);
	}

}
