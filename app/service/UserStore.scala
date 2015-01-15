package service

import java.util.{List => JList}
import javax.persistence.TypedQuery

import anorm.SqlParser._
import anorm._
import org.joda.time.DateTime
import play.api.Play.current
import play.api.db.DB
import play.api.{Application, Logger}
import securesocial.core.providers.Token
import securesocial.core.{IdentityId, _}
import utils.AnormExtension._
import plugins.DBUtil._
import models.Account

import scala.collection.JavaConversions._


class UserStore(application: Application) extends UserServicePlugin(application) {

  def resultToAccount(result: JList[Account]) : Option[Account] = {
    result.size() match {
      case 0 => None
      case 1 => Some(result.get(0))
      case _ => throw new RuntimeException("42. 31337. Should not happen!")
    }
  }

  def list() : Seq[Account] = {

    query { em =>
      val builder = em.getCriteriaBuilder
      val criteria = builder.createQuery(classOf[Account])
      val query = em.createQuery(criteria)

      asScalaBuffer(query.getResultList)
    }
  }

  def findAccount(id: IdentityId) : Option[Account] = {

    val user: Option[Account] = transaction { (em, tx) =>

      //for the userpass provider we want case insensitive lookup
      val queryStr = id.providerId match {
        case "userpass" =>
          """SELECT a from Account a WHERE
             LOWER(a.userid) = LOWER(:uid) AND a.provider = :provider"""
        case _ =>
          """SELECT a from Account a
             WHERE a.userid = :uid AND a.provider = :provider"""
      }

      val query : TypedQuery[Account] = em.createQuery(queryStr, classOf[Account])
      query.setParameter("uid", id.userId)
      query.setParameter("provider", id.providerId)
      resultToAccount(query.getResultList)
    }

    user
  }

  def findByEmail(email: String): List[Account] = {
    Logger.debug("findByEmail $email")

    transaction { (em, tx) =>
      val queryStr =
        """SELECT a from Account a
           WHERE LOWER(a.mail) = LOWER(:mail)"""

      val query : TypedQuery[Account] = em.createQuery(queryStr, classOf[Account])
      query.setParameter("mail", email)
      query.getResultList.toSet.toList
    }
  }

  // UserService implements

  def find(id: IdentityId): Option[Identity] = {
    Logger.debug("find")

    val account = findAccount(id)
    Logger.debug("found:" + account.toString)

    account
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug("findByEmailAndProvider $email, $providerId")

    transaction { (em, tx) =>
      val queryStr =
        """SELECT a from Account a
           WHERE LOWER(a.mail) = LOWER(:mail) AND a.provider = :provider"""

      val query : TypedQuery[Account] = em.createQuery(queryStr, classOf[Account])
      query.setParameter("mail", email)
      query.setParameter("provider", providerId)
      resultToAccount(query.getResultList)
    }
  }

  def save(user: Identity): Identity = {
    val dbUser: Option[Account] = findAccount(user.identityId)

    transaction { (em, tx) =>

      Logger.debug(dbUser.toString)

      dbUser match {

        case Some(account) =>
          Logger.debug("Have user already in the db!")
          account.updateFromIdentity(user)
          em.merge(account)
          account

        case None =>
          val account = Account(user)
          em.persist(account)
          Logger.debug("New user " + account.toString)
          account
      }
    }
  }

  private val tokenParser : anorm.RowParser[Token] = {
      get[String]("id") ~
      get[String]("email") ~
      get[DateTime]("creationTime") ~
      get[DateTime]("expirationTime") ~
      get[Boolean]("isSignUp") map {
      case i ~ e ~ ctime ~ etime ~ s => Token(i, e, ctime, etime, s)
    }
  }

  def save(token: Token) {

    DB.withConnection { implicit c =>
      SQL("""insert into Tokens(id, email, creationTime, expirationTime, isSignUp)
            |values({id}, {email}, {creationTime}, {expirationTime}, {isSignUp})
          """.stripMargin).onParams(token.uuid, token.email, token.creationTime,
                                    token.expirationTime, token.isSignUp).execute()
    }
  }

  def findToken(token: String): Option[Token] = {
    DB.withConnection{ implicit c =>
      SQL("select * from Tokens t where t.id = {uuid}").onParams(token).as(tokenParser.singleOpt)
    }
  }

  def deleteToken(uuid: String) {
    DB.withConnection{ implicit c =>
      SQL("delete from Tokens t where t.id = {uuid}").onParams(uuid).execute()
    }
  }

  def deleteTokens() {
    DB.withConnection{ implicit c =>
      SQL("delete from Tokens").execute()
    }
  }

  def deleteExpiredTokens() {
    DB.withConnection{ implicit c =>
      SQL("delete from Tokens t where t.expirationTime < now()").execute()
    }
  }
}