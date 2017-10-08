package sne.bid.test

import org.scalatest.FlatSpec

import sne.bid._

class BidMatcherSpec extends FlatSpec {
  
  // Some data
  
  val balance1 = new Balance
  balance1 put (Securities.DOLLAR, 1000)
  balance1 put (Securities.A, 10)
  balance1 put (Securities.B, 20)
  balance1 put (Securities.C, 30)
  balance1 put (Securities.D, 40)

  val balance2 = new Balance
  balance2 put (Securities.DOLLAR, 1000)
  balance2 put (Securities.A, 10)
  balance2 put (Securities.B, 20)
  balance2 put (Securities.C, 30)
  balance2 put (Securities.D, 40)

  val balance3 = new Balance
  balance3 put (Securities.DOLLAR, 900)
  balance3 put (Securities.A, 10)
  balance3 put (Securities.B, 20)
  balance3 put (Securities.C, 30)
  balance3 put (Securities.D, 44)
  
  // Test Securities

  "Securities" should "produce MatchError when constructed from an illegal string" in {
    assertThrows[MatchError] {
      val sec = Securities("Z")
    }
  }
  
  it should "not allow direct construction" in {
    assertDoesNotCompile("new Securities(\"A\")")
  }
  
  it should "support equality and hashCode" in {
    val sec1 = Securities("A")
    val sec2 = Securities("A")
    
    assert(sec1 === sec2)
    assert(sec1.hashCode === sec2.hashCode)
  }
  
  // Test Balance
  
  "Balance" should "support equality and hashCode" in {

    assert(balance1 === balance2)
    assert(balance1.hashCode === balance2.hashCode)
    assert(balance1 !== balance3)
  }
  
  it should "allow buy and sale operations" in {
    
    assert(balance1 === balance2)
    
    balance2 buy (Securities.A, 5, 50)
    assert(balance1 !== balance2)
    
    balance2 sale (Securities.A, 5, 50)
    assert(balance1 === balance2)
  }
  
  it should "support '+' and '-' operations" in {

    val balanceSum = balance1 + balance2
    val balanceSub = balanceSum - balance1
    assert(balance2 === balanceSub)
  }
  
  // Test Client
  
  "Client" can "buy and sale securities" in {

    val client1 = Client("Z1", balance1)
    client1 buy (Securities.D, 4, 100)
    assert(client1.balance === balance3)
    
    client1 sale (Securities.D, 4, 100)
    assert(client1.balance === balance1)
  }
  
  // Test BidMatcher
  
  "BidMatcher" should "process a bid" in {
    
    val clientName = "Z1"
    val client = Client(clientName, balance1)
    val clients = Map(clientName -> client)
    
    val bid_buy = Bid(clientName, "b", "D", 25, 4)
    BidMatcher.processBid(clients, bid_buy)
    assert(clients.get(clientName).get.balance === balance3)
    
    val bid_sale = Bid(clientName, "s", "D", 25, 4)
    BidMatcher.processBid(clients, bid_sale)
    assert(clients.get(clientName).get.balance === balance1)
  }
}