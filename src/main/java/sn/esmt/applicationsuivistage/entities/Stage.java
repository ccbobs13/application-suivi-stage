package sn.esmt.applicationsuivistage.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Data;
import lombok.NoArgsConstructor;
import sn.esmt.applicationsuivistage.entities.user.Etudiant;

@Entity
@Table(name = "stages")
@Data
@NoArgsConstructor
public class Stage implements Serializable {
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(length = 150, unique = true)
	@NotEmpty(message = "Le champs theme est requis")
	@Size(min = 10, max = 150, message = "Entrez entre 10 et 150 caracteres")
	private String theme;

	@OneToMany(mappedBy = "stage", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	private List<Etudiant> etudiants = new ArrayList<>();

}
