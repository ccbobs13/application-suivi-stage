package sn.esmt.applicationsuivistage.exceptions;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import lombok.Getter;
import lombok.Setter;

/**
 * @author ccb
 *
 */
@Getter
@Setter
@ResponseStatus(value = HttpStatus.BAD_REQUEST)
public class ResourceAlreadyExistsException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	private String resourceName;
	private String fieldName;
	private Object fieldValue;

	public ResourceAlreadyExistsException(String resourceName, String fieldName, Object fieldValue) {
		super(String.format("%s avec le champ %s: '%s' existe déjà", resourceName, fieldName, fieldValue));
		this.resourceName = resourceName;
		this.fieldName = fieldName;
		this.fieldValue = fieldValue;
	}

}
