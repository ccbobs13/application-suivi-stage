package sn.esmt.applicationsuivistage.exceptions;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingPathVariableException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import jakarta.validation.ConstraintViolationException;

/**
 * @author ccb
 *
 */
@ControllerAdvice
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {

	private Map<String, Object> body = new LinkedHashMap<>();

	
	@Override
	protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex,
			HttpHeaders headers, HttpStatusCode status, WebRequest request) {
		body.clear();
		body.put("timestamp", LocalDateTime.now());
		body.put("code", status.value());
		body.put("status", "KO");
		List<String> errors = ex.getBindingResult().getFieldErrors().stream().map(x -> x.getDefaultMessage())
				.collect(Collectors.toList());

		body.put("errors", errors);
		return ResponseEntity.status(status.value()).body(body);
	}

	/**
	 * Gestion de l'exception contraintViolation
	 * 
	 * @param ex
	 * @param request
	 * @return
	 */
	@ExceptionHandler(ConstraintViolationException.class)
	public ResponseEntity<Object> constraintViolationException(ConstraintViolationException ex, WebRequest request) {
		body.clear();
		body.put("timestamp", LocalDateTime.now());
		body.put("code", HttpStatus.BAD_REQUEST.value());
		body.put("status", "KO");

		List<String> errors = new ArrayList<>();
		ex.getConstraintViolations().forEach(cv -> errors.add(cv.getMessage()));

		body.put("errors", errors);

		return ResponseEntity.status(HttpStatus.BAD_REQUEST.value()).body(body);
	}
	
	@ExceptionHandler(IllegalArgumentException.class)
	public ResponseEntity<Object> illegalArgumentException(IllegalArgumentException ex, WebRequest request) {
		body.clear();
		body.put("timestamp", LocalDateTime.now());
		body.put("code", HttpStatus.BAD_REQUEST.value());
		body.put("status", "KO");

		body.put("error", ex.getMessage());

		return ResponseEntity.status(HttpStatus.BAD_REQUEST.value()).body(body);
	}

	@ExceptionHandler({ SQLException.class, DataIntegrityViolationException.class, DataAccessException.class })
	public ResponseEntity<Object> sqlException(Exception ex, WebRequest request) {
		body.clear();
		body.put("timestamp", LocalDateTime.now());
		body.put("code", HttpStatus.BAD_REQUEST.value());
		body.put("status", "KO");
		body.put("error", ex.getMessage());

		return ResponseEntity.status(HttpStatus.BAD_REQUEST.value()).body(body);
	}

	@Override
	protected ResponseEntity<Object> handleMissingPathVariable(MissingPathVariableException ex, HttpHeaders headers,
			HttpStatusCode status, WebRequest request) {
		// TODO Auto-generated method stub
		return super.handleMissingPathVariable(ex, headers, status, request);
	}

}
