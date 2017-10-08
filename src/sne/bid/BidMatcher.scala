package sne.bid

import java.io.File

import scala.collection.immutable.TreeMap
import scala.io.Source

/**
 * Collection of objects representing securities and currencies
 * used by bid matchers.
 */
class Securities private (value: String) {
  
  override def toString = value
}

object Securities {
  
  def apply(s: String) = s match {
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
    case "$" => DOLLAR
  }
  
  val DOLLAR = new Securities("$")
  val A = new Securities("A")
  val B = new Securities("B")
  val C = new Securities("C")
  val D = new Securities("D")
}

/**
 * Client balance - mapping: Securities -> amount
 */
class Balance extends scala.collection.mutable.HashMap[Securities, Long] {
  
  private def inc(sec: Securities, amount: Long) = put(sec, getOrElse(sec, 0L) + amount)
  private def dec(sec: Securities, amount: Long) = put(sec, getOrElse(sec, 0L) - amount) // FIXME "Для простоты можно не проверять отрицательные балансы клиентов по ценным бумагам и долларам."

  def buy(sec: Securities, amount: Long, buxes: Long): Unit = {
    dec(Securities.DOLLAR, buxes)
    inc(sec, amount)
  }
  
  def sale(sec: Securities, amount: Long, buxes: Long): Unit = {
    dec(sec, amount)
    inc(Securities.DOLLAR, buxes)
  }
  
  def +(that: Balance): Balance = {
    val ans = new Balance
    this.keys.foreach(k => ans.inc(k, this.get(k).get))
    that.keys.foreach(k => ans.inc(k, that.get(k).get))
    ans
  }
  
  def -(that: Balance): Balance = {
    val ans = new Balance
    this.keys.foreach(k => ans.inc(k, this.get(k).get))
    that.keys.foreach(k => ans.dec(k, that.get(k).get))
    ans
  }
  
  override def hashCode(): Int = {
    var hc = 0
    keys.foreach(k => (hc ^ k.hashCode) ^ get(k).get.hashCode)
    hc
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: Balance => {
      if (this.size == that.size) {
        !this.keys.exists(k => that.get(k) != this.get(k))
      }
      else {
        false
      }
    }
    case _ => false
  }
}

/**
 * One record of a 'clients' file.
 * Файл списка клиентов имеет следующие поля:
 * <ul>
 * <li>Имя клиента</li>
 * <li>Баланс клиента по долларам</li> 
 * <li>Баланс клиента по ценной бумаге "A" в штуках</li>
 * <li>Баланс по ценной бумаге "B"</li>
 * <li>Баланс по ценной бумаге "C"</li>
 * <li>Баланс по ценной бумаге "D"</li>
 * </ul>
 */
case class Client(name: String, balance: Balance) {

  /**
   * @param sec {@code Securities} to buy
   * @param amount amount to buy
   * @param buxes money to be paid
   */
  def buy(sec: Securities, amount: Long, buxes: Long): Unit = balance.buy(sec, amount, buxes)
  
  /**
   * @param sec {@code Securities} to sale
   * @param amount amount to sale
   * @param buxes income
   */
  def sale(sec: Securities, amount: Long, buxes: Long): Unit = balance.sale(sec, amount, buxes)

  override def toString(): String = {
    val sep = '\t'
    name + sep +
      balance.getOrElse(Securities.DOLLAR, 0) + sep +
      balance.getOrElse(Securities.A, 0) + sep +
      balance.getOrElse(Securities.B, 0) + sep +
      balance.getOrElse(Securities.C, 0) + sep +
      balance.getOrElse(Securities.D, 0)
  }
}

/**
 * One record of a 'bids' file.
 * Файл списка заявок имеет формат:
 * <ul>
 * <li>Имя клиента выставившего заявку</li>
 * <li>Символ операции: "s" - продажа или "b" - покупка.</li>
 * <li>Наименование ценной бумаги</li>
 * <li>Цена заявки (целое число за одну штуку ценной бумаги)</li>
 * <li>Количество продаваемых или покупаемых ценных бумаг</li>
 * </ul>
 */
case class Bid(clientName: String, operation: String, title: String, price: Long, amount: Long)

/**
 * The main bid matcher class. Initialized with the names of files for
 * clients, bids (orders), and results. After initialization, call {@code run()}
 * to execute the matching.
 */
class BidMatcher(clientsFilename: String, bidsFilename: String, resultsFilename: String) {
  
  /**
   * Execute the matching.
   */
  def run(): Unit = {
    
    val clients = loadClients()
    val bidsFile = Source.fromFile(bidsFilename)
    val lines = bidsFile.getLines()
    
    lines.foreach(ln => { val bid = buildBid(ln); if (bid.isDefined) BidMatcher.processBid(clients, bid.get)})
    
    bidsFile.close
    storeClients(clients)
  }
  
  private def buildBid(line: String): Option[Bid] = {
    
    val sep = '\t'
    val fields = line.split(sep)
    val bid = Bid(fields(0), fields(1), fields(2), fields(3).toLong, fields(4).toLong)
    Option(bid)
  }
  
  private def loadClients(): Map[String, Client] = {
    
    val sep = '\t'
    val inp = Source.fromFile(clientsFilename)
    try {
      def ans = for (line <- inp.getLines) yield {
        val fields = line.split(sep)
        val balance = new Balance
        balance put (Securities.DOLLAR, fields(1).toLong)
        balance put (Securities.A, fields(2).toLong)
        balance put (Securities.B, fields(3).toLong)
        balance put (Securities.C, fields(4).toLong)
        balance put (Securities.D, fields(5).toLong)
        (fields(0), Client(fields(0), balance))
      }
      ans.toMap
    }
    finally {
      inp.close
    }
  }
  
  private def storeClients(clients: Map[String, Client]): Unit = {
    
    val out = new java.io.PrintWriter(new java.io.File(resultsFilename))
    try {
      for (cl <- clients.values.toList.sortBy(_.name.substring(1).toLong)) out.println(cl)
    }
    finally {
      out.close
    }
  }
}

object BidMatcher {

  // Operation 'buy'
  val BUY = "b"
  // Operation 'sale'
  val SALE = "s"

  // Input: список клиентов биржи
  val CLIENTS_FILENAME = "clients.txt"
  // Input: список заявок от клиентов в хронологическом порядке
  val ORDERS_FILENAME = "orders.txt"
  // Output: состояние балансов всех клиентов после обработки всех заявок
  val RESULTS_FILENAME = "results.txt"
  // Default location of files.
  // CHANGE IT OR PROVIDE AN ARGUMENT
  val DEFAULT_PATH = "D:\\Task\\Matching"
  
  def main(args: Array[String]) {
    
    val (clientsFn, ordersFn, resultsFn) = constructFilenames(args)
    println(s"Clients $clientsFn")
    println(s"Orders  $ordersFn")
    println(s"Results $resultsFn")
    
    BidMatcher(clientsFn, ordersFn, resultsFn).run()
  }
  
  def apply(clientsFilename: String, bidsFilename: String, resultsFilename: String) = {
    new BidMatcher(clientsFilename, bidsFilename, resultsFilename)
  }
  
  private def constructFilenames(args: Array[String]): (String, String, String) = {

    val path = if (args.size > 0) args(0) else DEFAULT_PATH

    if (path == "-h") {
      println(s"""Usage: scala sne.bid.BidMatcher [path|-h]
                 |       path - path where $CLIENTS_FILENAME and $ORDERS_FILENAME are located
                 |       -h - print this text and exit""".stripMargin)
      System.exit(1)
    }
    
    (path+File.separator+CLIENTS_FILENAME, path+File.separator+ORDERS_FILENAME, path+File.separator+RESULTS_FILENAME)
  }
  
  /**
   * Process a single bid.
   * 
   * @param clients map of client names to {@link Client}
   * @param bid a bid to be processed
   */
  def processBid(clients: Map[String, Client], bid: Bid): Unit = {
    val clientOpt = clients.get(bid.clientName)
    if (clientOpt.isDefined) {
      val client = clientOpt.get
      
      val buxes = bid.price * bid.amount
      
      bid.operation match {
        case BUY  => client.balance.buy(Securities(bid.title), bid.amount, buxes)
        case SALE => client.balance.sale(Securities(bid.title), bid.amount, buxes)
      }
    }
  }
}