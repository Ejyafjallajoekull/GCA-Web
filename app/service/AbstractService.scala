// Copyright © 2014, German Neuroinformatics Node (G-Node)
//                   A. Stoewer (adrian.stoewer@rz.ifi.lmu.de)
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted under the terms of the BSD License. See
// LICENSE file in the root of the Project.

package service

import models._
import javax.persistence.{EntityNotFoundException, TypedQuery, Persistence, EntityManagerFactory}
import service.util.DBUtil
import scala.collection.JavaConversions._

/**
 * Service class that provides data access logic for abstracts and nested
 * authors and affiliations.
 */
class AbstractService(val emf: EntityManagerFactory) extends DBUtil {

  /**
   * List all published abstracts that belong to a conference.
   *
   * @param conference The conference for which to list the abstracts.
   *
   * @return All published abstracts that are associated with a
   *         certain conference.
   */
  def list(conference: Conference) : Seq[Abstract] = {
    dbQuery { em =>
      val queryStr =
        """SELECT DISTINCT a FROM Abstract a
           LEFT JOIN FETCH a.owners
           LEFT JOIN FETCH a.authors
           LEFT JOIN FETCH a.affiliations
           LEFT JOIN FETCH a.conference c
           LEFT JOIN FETCH a.figure
           LEFT JOIN FETCH a.references
           WHERE c.uuid = :uuid"""

      val query: TypedQuery[Abstract] = em.createQuery(queryStr, classOf[Abstract])
      query.setParameter("uuid", conference.uuid)
      asScalaBuffer(query.getResultList)
    }
  }

  /**
   * List all published and unpublished abstracts that belong to an account.
   *
   * @param account The account for which to list the abstracts.
   *
   * @return All abstracts that belong to an account.
   */
  def listOwn(account: Account) : Seq[Abstract] = {
    dbQuery { em =>
      val queryStr =
        """SELECT DISTINCT a FROM Abstract a
           LEFT JOIN FETCH a.owners o
           LEFT JOIN FETCH a.authors
           LEFT JOIN FETCH a.affiliations
           LEFT JOIN FETCH a.conference
           LEFT JOIN FETCH a.figure
           LEFT JOIN FETCH a.references
           WHERE o.uuid = :uuid"""

      val query: TypedQuery[Abstract] = em.createQuery(queryStr, classOf[Abstract])
      query.setParameter("uuid", account.uuid)
      asScalaBuffer(query.getResultList)
    }
  }

  /**
   * Return a published abstract by id.
   *
   * @param id The id of the abstract.
   *
   * @return The abstract with the specified id.
   *
   * @throws NoResultException If the conference was not found
   */
  def get(id: String) : Abstract= {
    dbQuery { em =>
      val queryStr =
        """SELECT DISTINCT a FROM Abstract a
           LEFT JOIN FETCH a.owners
           LEFT JOIN FETCH a.authors
           LEFT JOIN FETCH a.affiliations
           LEFT JOIN FETCH a.conference
           LEFT JOIN FETCH a.figure
           LEFT JOIN FETCH a.references
           WHERE a.published = true AND a.uuid = :uuid"""

      val query: TypedQuery[Abstract] = em.createQuery(queryStr, classOf[Abstract])
      query.setParameter("uuid", id)
      query.getSingleResult
    }
  }

  /**
   * Return an abstract with a certain id, that is owned by an account.
   * The abstract doesnt need to be published if the account is an owner
   * of the abstract.
   *
   * @param id      The id of the abstract.
   * @param account The account who wants to request the abstract.
   *
   * @return The abstract with the specified id.
   *
   * @throws EntityNotFoundException If the account does not exist.
   *
   * @throws NoResultException If the conference was not found
   */
  def getOwn(id: String, account: Account) : Abstract = {
    dbQuery { em =>
      val queryStr =
        """SELECT DISTINCT a FROM Abstract a
           LEFT JOIN FETCH a.owners o
           LEFT JOIN FETCH a.authors
           LEFT JOIN FETCH a.affiliations
           LEFT JOIN FETCH a.conference
           LEFT JOIN FETCH a.figure
           LEFT JOIN FETCH a.references
           WHERE o.uuid = :owneruuid AND a.uuid = :uuid"""

      val accountChecked = em.find(classOf[Account], account.uuid)
      if (accountChecked == null)
        throw new EntityNotFoundException("Unable to find account with uuid = " + account.uuid)

      val query: TypedQuery[Abstract] = em.createQuery(queryStr, classOf[Abstract])
      query.setParameter("owneruuid", account.uuid)
      query.setParameter("uuid", id)
      query.getSingleResult
    }
  }

  /**
   * Create a new abstract.
   * This is only permitted if the account is one of the owners.
   *
   *
   * @param abstr   The Abstract to create.
   * @param conference  The the id of the conference.
   * @param account The account who wants to perform the creation.
   *
   * @return The created and persisted abstract.
   */
  def create(abstr : Abstract, conference: Conference, account: Account) : Abstract = {
    val abstrCreated = dbTransaction { (em, tx) =>

      val accountChecked = em.find(classOf[Account], account.uuid)
      if (accountChecked == null)
        throw new EntityNotFoundException("Unable to find account with uuid = " + account.uuid)

      val conferenceChecked = em.find(classOf[Conference], conference.uuid)
      if (conferenceChecked == null)
        throw new EntityNotFoundException("Unable to find conference with uuid = " + conference.uuid)

      if (abstr.uuid != null)
        throw new IllegalArgumentException("Unable to create an abstract with not null uuid")

      abstr.conference = conferenceChecked
      abstr.owners.add(accountChecked)

      abstr.authors.foreach { author =>
        author.abstr = abstr
      }

      abstr.affiliations.foreach { affiliation =>
        affiliation.abstr = abstr
      }

      abstr.references.foreach { reference =>
        reference.abstr = abstr
      }

      em.merge(abstr)
    }

    getOwn(abstrCreated.uuid, account)
  }

  /**
   * Update an existing abstract.
   * This is only permitted if the account is one of the owners.
   *
   * @param abstr   The Abstract to update.
   * @param account The account who wants to perform the update.
   *
   * @return The updated and persisted abstract.
   */
  def update(abstr : Abstract, account: Account) : Abstract = {
    val abstrUpdated = dbTransaction { (em, tx) =>

      if (abstr.uuid == null)
        throw new IllegalArgumentException("Unable to update an abstract with null uuid")

      val abstrChecked = em.find(classOf[Abstract], abstr.uuid)
      if (abstrChecked == null)
        throw new EntityNotFoundException("Unable to find abstract with uuid = " + abstr.uuid)

      val accountChecked = em.find(classOf[Account], account.uuid)
      if (accountChecked == null)
        throw new EntityNotFoundException("Unable to find account with uuid = " + account.uuid)

      if (!abstrChecked.owners.contains(accountChecked))
        throw new IllegalAccessException("No permissions for abstract with uuid = " + abstr.uuid)

      abstr.authors.foreach { author =>
        author.abstr = abstr
      }

      abstr.affiliations.foreach { affiliation =>
        affiliation.abstr = abstr
      }

      abstr.references.foreach { reference =>
        reference.abstr = abstr
      }

      val merged = em.merge(abstr)

      abstrChecked.authors.foreach { author =>
        if (!abstr.authors.contains(author))
          em.remove(author)
      }

      abstrChecked.affiliations.foreach { affiliation =>
        if (!abstr.affiliations.contains(affiliation))
          em.remove(affiliation)
      }

      abstrChecked.references.foreach { reference =>
        if (!abstr.references.contains(reference))
          em.remove(reference)
      }

      merged
    }

    getOwn(abstrUpdated.uuid, account)
  }

  /**
   * Delete an abstract.
   * This is only permitted if the account is one of the owners.
   *
   * @param id      The id of the abstract to delete.
   * @param account The account who wants to perform the delete.
   *
   * @throws IllegalArgumentException If the conference has no uuid
   * @throws EntityNotFoundException If the conference or the user does not exist
   * @throws IllegalAccessException If account is not an owner.
   */
  def delete(id: String, account: Account) : Unit = {
    dbTransaction { (em, tx) =>

      val accountChecked = em.find(classOf[Account], account.uuid)
      if (accountChecked == null)
        throw new EntityNotFoundException("Unable to find account with uuid = " + account.uuid)

      val abstrChecked = em.find(classOf[Abstract], id)
      if (abstrChecked == null)
        throw new EntityNotFoundException("Unable to find abstract with uuid = " + id)

      if (!abstrChecked.owners.contains(accountChecked))
        throw new IllegalAccessException("No permissions for abstract with uuid = " + id)

      abstrChecked.authors.foreach(em.remove(_))
      abstrChecked.affiliations.foreach(em.remove(_))
      abstrChecked.references.foreach(em.remove(_))

      em.remove(abstrChecked)
    }
  }

}


object AbstractService {

  def apply() : AbstractService = {
    new AbstractService(
      Persistence.createEntityManagerFactory("defaultPersistenceUnit")
    )
  }

}
