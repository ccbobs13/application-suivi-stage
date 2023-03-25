package sn.esmt.applicationsuivistage.entities.user;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "encadreurs")
@Data
@NoArgsConstructor
public class Encadreur implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	@Email(message = "Email invalide")
	@NotEmpty(message = "L''email est obligatoire")
	@Column(unique = true)
	private String email;
	
	@Column(length = 30)
	@NotEmpty(message = "Le nom est obligatoire")
	private String nom;
	
	@Column(length = 30)
	@NotEmpty(message = "Le prénom est obligatoire")
	private String prenoms;
	
	@Column(length = 30)
	@NotEmpty(message = "Le grade est obligatoire")
	private String grade;

	@OneToMany(mappedBy = "encadreur", fetch = FetchType.EAGER)
	private List<Etudiant> etudiants = new ArrayList<>();

}
