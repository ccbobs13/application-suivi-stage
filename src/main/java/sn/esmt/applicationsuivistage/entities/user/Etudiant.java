package sn.esmt.applicationsuivistage.entities.user;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import sn.esmt.applicationsuivistage.entities.Stage;

@Entity
@Table(name = "etudiants")
@Data
@NoArgsConstructor
public class Etudiant implements Serializable {

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
	
	@ManyToOne
	@JoinColumn(name = "id_encadreur")
//	@NotNull(message = "L''encadreur est obligatoire")
	// Pour eviter la boucle infinie du @ToString de Lombok sur les relations many
	@ToString.Exclude
	private Encadreur encadreur;

	@ManyToOne
	@JoinColumn(name = "id_stage")
	@NotNull(message = "Le stage est obligatoire")
	@ToString.Exclude
	private Stage stage;

}
