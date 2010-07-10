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

(values
 VALUE* | NAMED-VALUE*)

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
	      (let ((type (canonical-type-name system (first specification))))
		(named-let loop-entities ((entities (rest specification)))
		  (if entities
		      (progn
			(add-entity system (first entities) type unknown)
			(loop-entities (rest entities))))))
	      (add-entity system specification nil unknown))
	  (loop-specs (rest specifications))))))

(defun ensure-entity (system entity type unknown)
  (if (has-entity-p system entity)
      (unless (let ((stored-type (entity-type (get-entity system entity))))
		(or (not stored-type) (eq type stored-type)))
	(error 'gcs-type-error))
      (add-entity system entity type unknown)))

(defun ensure-constraints-entities (system specifications)
  (dolist (specification specifications)
    (map nil (lambda (entity type)
	       (ensure-entity system entity type nil))
	 (rest specification)
	 (get-predicate-signature (gcs-universe system) (first specification)))))

(defun make-criteria (universe specifications)
  "Returns the list of functions that will evaluate the fitness of
a solution, with respect to each constraint.

SPECIFICATIONS is the constraints part of the system definition."
  (mapcar (lambda (specification)
	    (funcall (get-predicate-criterion universe (first specification)) (rest specification)))
	  specifications))

(defun read-gcs (form universe)
  (let* ((options (rest form))
	 (constraints (rest (assoc 'constraints options)))
	 (parameters (rest (assoc 'parameters options)))
	 (unknowns (rest (assoc 'unknowns options)))
	 (system (make-instance 'geometrical-constraints-system :universe universe)))
    (add-entities-from-specifications system unknowns t)
    (add-entities-from-specifications system parameters nil)
    (ensure-constraints-entities system constraints)
    (values system (make-criteria (gcs-universe system) constraints))))
