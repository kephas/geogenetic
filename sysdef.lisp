(in-package :thierry-technologies.com/2010/06/gene-gcs)

#|

System definition
=================

A system definition file contains a series of forms defining systems
and valuations. All forms are independent.


A system definition form is as follows:

(gcs
 OPTIONS*)

OPTIONS = CONSTRAINTS-OPT | PARAMETERS-OPT | UNKNOWNS-OPT ;

CONSTRAINTS-OPT = (constraints CONSTRAINT*)
CONSTRAINT = (PREDICATE ENTITY*)

PARAMETERS-OPT = (parameters ENTITY-SPEC*)
UNKNOWNS-OPT = (unknowns ENTITY-SPEC*)

ENTITY-SPEC = ENTITY | (TYPE ENTITY*)

PREDICATE, ENTTY and TYPE are symbols. Specifying the type of an entity
is not needed if it can be infered from its use in a constraint. (e.g. if
there is a constraint (distcc x y z) and the signature of distcc is
(distance circle circle), then x will be known as a distance)


A valuation definition form is as follows:

(bare-values VALUE*) | (named-values NAMED-VALUE*)

NAMED-VALUE = (ENTITY VALUE)

VALUE may be any form that can be read by the value function for the type the
corresponding entity.

If the values are provided without name, they must be in the same order and of
the same number than in the parameters option of the system definition for which
they are used.

|#

(defun add-entities-from-specifications (system specifications unknown)
  "Adds the entities from the parameter or unknown SPECIFICATIONS."
  (named-let loop-specs ((specifications specifications))
    (if specifications
	(let ((specification (first specifications)))
	  (if (listp specification)
	      (let ((type (first specification)))
		(named-let loop-entities ((entities (rest specification)))
		  (if entities
		      (progn
			(add-entity system (first entities) type unknown)
			(loop-entities (rest entities))))))
	      (add-entity system specification nil unknown))
	  (loop-specs (rest specifications))))))

(defun ensure-entity (system entity type unknown)
  (cif object (get-entity system entity)
       (cif stored-type (entity-type (get-entity system entity))
	    (unless (eq type stored-type)
	      (error 'gcs-type-error))
	    (setf (slot-value object 'type) type))
       (add-entity system entity type unknown)))

(defun ensure-constraints-entities (system specifications)
  (dolist (specification specifications)
    (map nil (lambda (entity type)
	       (ensure-entity system entity type t))
	 (rest specification)
	 (get-predicate-signature (gcs-universe system) (first specification)))))

(defun make-criteria (universe specifications)
  "Returns the list of functions that will evaluate the fitness of
a solution, with respect to each constraint.

SPECIFICATIONS is the constraints part of the system definition."
  (mapcar (lambda (specification)
	    (funcall (get-predicate-criterion universe (first specification)) (rest specification)))
	  specifications))

(defun untype-parameters-specifications (specifications)
  "Returns the list of paramters' names, stripped from the type information
if present in SPECIFICATIONS."
  (mapcan (lambda (specification)
	    (if (listp specification)
		(rest specification)
		(list specification)))
	  specifications))

(defun read-gcs (form universe)
  (let* ((options (rest form))
	 (constraints (rest (assoc 'constraints options)))
	 (parameters (rest (assoc 'parameters options)))
	 (unknowns (rest (assoc 'unknowns options)))
	 (system (make-instance 'geometrical-constraints-system
				:universe universe
				:params (untype-parameters-specifications parameters))))
    (add-entities-from-specifications system unknowns t)
    (add-entities-from-specifications system parameters nil)
    (ensure-constraints-entities system constraints)
    (values system (make-criteria (gcs-universe system) constraints))))

(defun read-bared-values (form universe)
  (declare (ignore universe))
  (make-instance 'sequential-valuation :values (rest form)))

(defun read-named-values (form universe)
  (declare (ignore universe))
  (let ((valuation (make-instance 'named-valuation)))
    (dolist (named-value (rest form) valuation)
      (store-named-value valuation (first named-value) (second named-value)))))

(defvar *reader-associations*
  `((gcs ,#'read-gcs)
    (bare-values ,#'read-bared-values)
    (named-values ,#'read-named-values)))

(defun read-definitions-files (pathname universe)
  (let ((*package* (find-package :thierry-technologies.com/2010/06/gene-gcs)))
    (with-open-file (in pathname)
      (named-let rec ((form (read in nil))
		       (acc))
	(if form
	    (let* ((kind (first form))
		   (reader-function (cif fun (second (assoc kind *reader-associations*))
					fun
					(constantly nil))))
	      (rec (read in nil) (cons (cons kind (multiple-value-list (funcall reader-function form universe))) acc)))
	    (reverse acc))))))
