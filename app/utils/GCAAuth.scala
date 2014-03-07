package utils

import play.api.mvc._
import scala.concurrent.Future
import securesocial.core.{Authenticator, IdentityProvider, UserService, SecureSocial}
import models.Account
import javax.persistence.NoResultException
import scala.Some
import play.api.mvc.SimpleResult
import play.api.libs.json.Json
import securesocial.core.providers.utils.RoutesHelper
import play.api.i18n.Messages
import scala.language.higherKinds


case class RequestWithAccount[A](user: Option[Account], request: Request[A]) extends WrappedRequest(request)
case class RequestAuthenticated[A](user: Account, request: Request[A]) extends WrappedRequest(request)

trait GCAAuth extends securesocial.core.SecureSocial {

  trait SafeActionBuilder[R[_]] extends ActionBuilder[R] {

    var isAjaxCall = false

    def invokeBlockSafe[A](resultAsRest: Boolean)(request: R[A],
                                                block: (R[A]) => Future[SimpleResult]): Future[SimpleResult] = {
      try {
        block(request)
      } catch (if(resultAsRest) {
        exHandlerJSON()
      } else {
        exHandlerHTML()
      }) andThen {
        result => Future.successful(result)
      }

    }

    final def apply(isREST: Boolean)(block: R[AnyContent] => Result): Action[AnyContent] = {
      isAjaxCall = isREST
      apply(BodyParsers.parse.anyContent)(block)
    }
  }

  object AccountAwareAction extends SafeActionBuilder[RequestWithAccount] {
    protected def invokeBlock[A](request: Request[A],
                                 block: (RequestWithAccount[A]) => Future[SimpleResult]): Future[SimpleResult] = {
      val Account = getAccount(request)
      invokeBlockSafe(resultAsRest = isAjaxCall)(RequestWithAccount(Account, request), block)
    }
  }

  object AuthenticatedAction extends SafeActionBuilder[RequestAuthenticated] {

    protected def invokeBlock[A](request: Request[A],
                                 block: (RequestAuthenticated[A]) => Future[SimpleResult]): Future[SimpleResult] = {
      val Account = getAccount(request)

      Account map { account =>
        invokeBlockSafe(resultAsRest = isAjaxCall)(RequestAuthenticated(account, request), block)
      } getOrElse {
        authFailedResponse(request)
      }
    }

    def authFailedResponse[A](implicit request: Request[A]) : Future[SimpleResult] = {
      val response = if (isAjaxCall) {
        Unauthorized(Json.toJson(Map("error"->"Credentials required"))).as(JSON)
      } else {
        Redirect(RoutesHelper.login().absoluteURL(IdentityProvider.sslEnabled))
          .flashing("error" -> Messages("securesocial.loginRequired"))
          .withSession(session + (SecureSocial.OriginalUrlKey -> request.uri))

      }

      Future.successful(response.discardingCookies(Authenticator.discardingCookie))
    }
  }

  def getAccount[A](request: Request[A]): Option[Account] = {
    implicit val req = request
    for {
      authenticator <- SecureSocial.authenticatorFromRequest
      user <- UserService.find(authenticator.identityId)
      account <- user match {case a: Account => Some(a); case _ => None}
    } yield {
      touch(authenticator)
      account
    }
  }

  def exHandlerHTML() : PartialFunction[Throwable, SimpleResult] = {
    case e: NoResultException => InternalServerError("No Result !\n" + e.getStackTraceString)
    case e: Exception => InternalServerError("<html><h1>Uh oh!</h1><br/>\n" + e.getStackTraceString + "</html>")
  }

  def exHandlerJSON() : PartialFunction[Throwable, SimpleResult] = {
    case e: NoResultException => InternalServerError("No Result !\n" + e.getStackTraceString)
    case e: Exception => InternalServerError("Uh oh!\n" + e.getStackTraceString)
  }
}