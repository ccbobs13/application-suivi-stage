package sn.esmt.applicationsuivistage.enums;

public enum ErrorType {
	
	CHARGEMENT_ERROR("Une erreur s'est produite lors du chargement"),
	MODIFICATION_ERROR("Une erreur s'est produite lors de la modification"),
	CREATION_ERROR("Une erreur s'est produite lors de la création"),
	SUPPRESSION_ERROR("Une erreur s'est produite lors de la suppression");

	private final String message;

	ErrorType(String message) {
		this.message = message;
	}

	public String get() {
		return message;
	}
}
