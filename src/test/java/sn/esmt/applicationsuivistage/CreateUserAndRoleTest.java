package sn.esmt.applicationsuivistage;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import sn.esmt.applicationsuivistage.entities.Role;
import sn.esmt.applicationsuivistage.entities.user.User;
import sn.esmt.applicationsuivistage.repositories.RoleRepository;
import sn.esmt.applicationsuivistage.repositories.UserRepository;
import sn.esmt.applicationsuivistage.services.RoleService;
import sn.esmt.applicationsuivistage.services.UserService;

@SpringBootTest
class CreateUserAndRoleTest {

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private RoleRepository roleRepository;

	@Autowired
	private UserService userService;

	@Autowired
	private RoleService roleService;

	@Value("${application-suivi-stage.admin}")
	private String adminEmail;

	@Value("${application-suivi-stage.password}")
	private String adminPwd;

	@Test
	void createRoles() {
		if (!roleRepository.existsByNom("ROLE_ADMIN")) {
			roleService.create("ROLE_ADMIN");
		}

		if (!roleRepository.existsByNom("ROLE_USER")) {
			roleService.create("ROLE_USER");
		}
	}

	@Test
	void createUser() {
		createRoles();

		if (!userRepository.existsByEmail(adminEmail)) {
			User admin = new User();
			admin.setEmail(adminEmail);
			admin.setPassword(adminPwd);

			userService.create(admin, "ROLE_ADMIN");
		}
	}

	@Test
	void infiniteLoopForRoleWhileUsingToStringOnManyRelationEntitiesTest() {
		List<Role> roles = roleRepository.findAll();
		roles.forEach(e -> System.out.println(e));
	}

	@Test
	void createTestUser() {
		if (!userRepository.existsByEmail("testeur@esmt.sn")) {
			User user = new User();
			user.setEmail("testeur@esmt.sn");
			user.setPassword("testeur");
			userService.create(user, "ROLE_USER");
		}
	}

}
