package sn.esmt.applicationsuivistage.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import sn.esmt.applicationsuivistage.entities.user.User;

@Data
@NoArgsConstructor
@Entity
@Table(name = "roles")
public class Role implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(length = 25, unique = true)
	private String nom;

	@ManyToMany(mappedBy = "roles")
	@ToString.Exclude
	private List<User> users = new ArrayList<>();

}
