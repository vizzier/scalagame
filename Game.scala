
package cw
import scala.util.control._
import scala.collection.mutable.ListBuffer
/**
 * This class holds an instance of a simple game where
 * a player moves on a field and collects coins.
 * See the explanation sheet and comments in this file for details. The constructor builds an
 * instance of a game using the accepted parameters.
 *
 * @param wall A list of coordinates (as tuples) where walls exist. Example: The parameter List((0,0),(0,1)) puts two wall elements in the upper left corner and the position below.
 * @param coin A list of coins, each is a position and a value (i.e. a 3 value tuple). Example: List((0,0,50)) puts a coin in the upper left corner which adds 50 to the score.
 * @param initialX The initial x position of the player.
 * @param initialY The initial y position of the player. If initialX and initialY are 0, the player starts in the upper left corner.
 */
class Game(wall: List[(Int, Int)], coin: List[(Int, Int, Int)], initialX: Int, initialY: Int) {

  //the current grid, a 10x10 field, where -1=empty, 0=wall, any positive number=coin
  private var field: Array[Array[Int]] = Array.ofDim[Int](10, 10)

  /* Please note - to align with the overall case study (see explanation sheet), both of the above two-dimensional arrays
   * should be accessed in the format field(col)(row) so field(2)(0) would retrieve the 3rd column and the 1st row (as indexing starts at zero),
   * equivalent to an (x,y) coordinate of (2,0). You may therefore visualise each inner array as representing a column of data.
   */

  //the current score, initially 0
  private var score: Int = 0
  //the current player position. As the player moves these positions update.
  private var positionX: Int = initialX
  private var positionY: Int = initialY
  //the current X and Y save position, initially -1
  private var saveX: Int = -1
  private var saveY: Int = -1
  private var sum =0
  /* This code is executed as part of the constructor. It firstly initialises all cells to -1 (i.e. empty).
   * It uses the list of walls provided to initialise the walls in the field array by setting given coordinates to 0.
   * It then uses the list of coins to initialise the coins in the field array by setting given coordinates to the provided number.
   */
  for (i <- 0 until 10; k <- 0 until 10) field(i)(k) = -1
  wall.foreach(w => field(w._1)(w._2) = 0)
  coin.foreach(w => field(w._1)(w._2) = w._3)
  coin.foreach(w => sum=sum+w._3)
    
  /**
   * Repeatedly run a sequence of commands. For example:
   *    for(i <- 1 to 5) println("Hello")
   * can be replaced by
   *    rpt(5)(println("Hello"))
   */
  def rpt(n: Int)(commands: => Unit) {
    for (i <- 1 to n) { commands }
  }
  
  /**
   * Utilised for GameApp.scala
   */
  def printField(): Unit ={
    for(k<-0 until 10){
      for(i<-0 until 10){
        if(positionX==i && positionY==k){
          print("p")
        }else if(saveX==i && saveY==k){
          print("s")
        }else if(field(i)(k)== 0){
          print("w")
        }else if(field(i)(k)== -1){
          print(".")
        }else{
          print(field(i)(k))
        }
      }
      println()
    }
  }
def iswall(i:Int,j:Int) : Boolean ={
  if (field(i)(j)==0)
   return true 
  else  return false
}
/********************************************************************************
   * COURSEWORK STARTS HERE - COMPLETE THE DEFINITIONS OF EACH OF THE OPERATIONS
   * WE SUGGEST YOU RUN THE GameTest SUITE AFTER EVERY CHANGE YOU MAKE TO THESE
   * SO YOU CAN SEE PROGRESS AND CHECK THAT YOU'VE NOT BROKEN ANYTHING THAT USED
   * TO WORK.
   *******************************************************************************/

  /**
   * Returns the current position of the player as a tuple, in (x,y) order.
   */
  def getPlayerPos(): (Int, Int) = {
    return (positionX, positionY);
  }

  /**
   * Returns the current score.
   */
  def getScore(): Int = score;

  /**
   * Move the player one place to the left.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft() {
    if(positionX!=0){
    positionX=positionX-1
    if(!iswall(positionX,positionY))
    {
       checkCoin()
     }
       else {
         positionX=positionX+1
             }
      } 
      checkCoins()
 }
  

  /**
   * Move the player one place to the right.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight() {
     if(positionX!=9){
    positionX=positionX+1
    if(!iswall(positionX,positionY)){
       checkCoin()
    }
    else {
     positionX=positionX-1
    }
    } 
    checkCoins()
   }
 
  

  /**
   * Move the player one place up.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp() {
   if(positionY!=0){
    positionY=positionY-1
    if(!iswall(positionX,positionY)){
       checkCoin()
    }
    else {
     positionY=positionY+1
    }
   } 
    checkCoins()
 }
  
  

  /**
   * Move the player one place down.
   * If there is a wall or the field ends, nothing happens.
   * If there is a coin, it is collected (i.e. a call to checkCoin() is made).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown() {
    if(positionY!=9){
    positionY=positionY+1
    if(!iswall(positionX,positionY)){
       checkCoin()
    }
    else {
     positionY=positionY-1
    }
    } 
    checkCoins()
 }
  
  

  /**
   * Move the player n places to the left. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveLeft(n: Int) {
    if (n>0){
      rpt(n){
        moveLeft()
      }
      checkCoins()
    }
  }
  

  /**
   * Move the player n places to the right. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveRight(n: Int) {
    if (n>0){
      rpt(n){
        moveRight()
      }
      checkCoins()
    }
  }
  

  /**
   * Move the player n places up. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveUp(n: Int) {
    if(n>0){
      rpt(n){
        moveUp()
      }
      checkCoins()
    }
  }
  

  /**
   * Move the player n places down. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any coins are collected (i.e. a call to checkCoin() is made after each move).
   * A more advanced requirement would be to call checkCoins() if completed.
   */
  def moveDown(n:Int) {
    if (n>0){
      rpt(n){
        moveDown()
      }
      checkCoins()
    }
  }
  

  /**
   * Checks if the current position is a coin. A coin exists if the cell
   * has a value larger than 0. If a coin does exist, increase the score,
   * and then erase the coin, i.e. set it to -1.
   */
  def checkCoin() {
        if ((field(positionX)(positionY))>0){
          score=score+(field(positionX)(positionY))
          field(positionX)(positionY) = -1
          
        }
      }

  //The methods beyond this point (aside to those in GameBuilder which is a separate task) are more complex than those above.

  /**
   * This moves the player according to a string. The string can contain the
   * letters w, a, s, d representing up, left, down, right moves. If
   * there is a wall or the field ends, the individual move is not
   * executed. Any further moves are done. Any coins are collected and the
   * save position is evaluated.
   */
  def move(s: String) {
       for(i <- 0 to (s.length-1)){
         s(i).toUpper match {
           case 'W' => moveUp()
           case 'A' => moveLeft()
           case 'S' => moveDown()
           case 'D' => moveRight()
         }
       }
    
    checkCoins()
  }
 
  /**
   * Identifies the maximum overall score in the game. This is the sum
   * of the current score and the possible score from collecting all of the remaining coins.
   * No coins are collected here, only the max score is returned.
   */
  def maxScore(): Int = sum
      

  /**
   * Checks if the rectangle defined by the current position and saved position
   * covers nine or more positions. If yes, it collects coins in it, increases the
   * score, and erases the coins. Also resets the saved position to -1,-1.
   */
  def checkCoins() {
    var a = 0
    var csvx = saveX
    var csvy = saveY
    var corners = List((positionX.max(saveX),positionY.min(saveY)),(positionX.min(saveX),positionY.max(saveY)))
    if(getSavePos!=(-1,-1)&& saveX!=positionX && saveY!=positionY){
      a = ((positionX-getSavePos._1).abs+(positionY-getSavePos._2).abs)*2
    }
    if (a>=9 ) {
      saveX = -1 
      saveY = -1
      
      }
    if(positionX>(-1) && csvx>(-1)){for (i<-csvx to positionX){
      for (j<-csvy to positionY){
        if (field(i)(j)>0 && !corners.contains((i,j))){
          score = score+field(i)(j)
          field(i)(j)= (-1)
        }
      }
    }}
    
  }

  /**
   * This gives a string in the format for move, which collects all the available coins. No specific
   * requirements for the efficiency of the solution exist, but the solution must consist of a finite number
   * of steps. The move is combined of a number of moves given by suggestMove.
   * If these are not possible, an empty string is returned. No coins are collected
   * and the player must be at the original position after the execution of the method.
   */
  def suggestSolution(): String = {
    var px = positionX
    var py = positionY
    var l = coin.length
    var c2 = ListBuffer[(Int,Int,Int)]()
    for(i<-coin.length-1 to 0 by -1){
      c2+=coin(i)
    }
    
    var a = ListBuffer[String]()
    var i =0
   while(i<c2.length){
     var c = suggestMove(c2(i)._1,c2(i)._2)
     if (!c.isEmpty){
       positionX=c2(i)._1
       positionY=c2(i)._2
       a+=c
       c2-=c2(i)
       i =0
     }
     else  i+=1
   }
      
     positionX=px
     positionY=py
    return a.mkString
  }

  /**
   * This gives a string in the format for move, which moves from the current position to
   * position x,y. No specific requirements for the efficiency of the solution exist. The move
   * cannot jump walls. The method is restricted to finding a path which is combined of a number of
   * left and then a number of up movement, or left/down, or right/up, or right/down movements only.
   * If this is not possible due to walls, it returns an empty string. No actual move is done. If
   * x or y are outside the field, an empty string is returned as well.
   */
  def suggestMove(x: Int, y: Int): String = {
    var mh = ListBuffer[String]()
    var checker1=0
    var checker2=0
    var checker3=0
    var checker4=0
    var sol = ""
  var px = getPlayerPos._1
  var py = getPlayerPos._2
  if(x>9||x<0||y>9||y<0||x==positionX||y==positionY)
  {
    return ""
   }
  for(i<-px.min(x) to x.max(px)){
    if(field(i)(py)==0)checker1=checker1+1
  }
    for(i<-py.min(y) to y.max(py)){
    if(field(px)(i)==0)checker2=checker2+1
  }
    for(i<-px.min(x) to x.max(px)){
    if(field(i)(y)==0)checker3=checker3+1
  }
    for(i<-py.min(y) to py.max(y)){
    if(field(x)(i)==0)checker4=checker4+1
  }
  if(checker1>0&&checker2>0)return""
  else if(checker4>0&&checker3>0)return""
  else if(checker2>0&&checker4>0)return""
  else if(checker4>0||checker1>0){
   if(checker2>0||checker3>0)return""}
 
  
    if(px<x){
        mh+= "d"*(x-px)
        }
     
    else {
        mh+= "a"*(px-x)
        }
     
    if(py<y){
        mh+= "s"*(y-py)
       }
      else {
        mh+= "w"*(py-y)
      }
    sol= mh.mkString

  if(checker1==0&&checker4==0){
           return sol}
  else if(checker2==0&&checker3==0)
  { return sol.reverse }
  else return ""
  }
 
 
 


  
  /* --- The three save methods below are used by the unit tests to simulate certain conditions --- */

  /**
   * Updates saveX and saveY to the current player position.
   */
  def save(): Unit = {
    /* This method is already implemented. You should not change it */
    saveX = positionX
    saveY = positionY
  }

  /**
   * Returns the current save position as a tuple, in (x,y) order.
   */
  def getSavePos(): (Int, Int) = {
    /* This method is already implemented. You should not change it */
    return (saveX, saveY);
  }

  /**
   * Sets the savePos to the values of the parameters.
   */
  def setSavePos(saveX: Int, saveY: Int): Unit = {
    /* This method is already implemented. You should not change it */
    this.saveX = saveX
    this.saveY = saveY
  }

}

/**
 * This object builds and returns a standard instance of Game.
 * It is used by the unit tests to initialise the game in different states.
 * Currently, there are three ways in which a game can be initialised,
 * the first has been completed but the other two initialisation methods need writing.
 */
object GameBuilder {

  /**
   * @return A game with
   * - walls in positions 3,0 3,1 and 3,2
   * - a coin at 4,1 which increases score by 100
   * - a coin at 3,3 which increases score by 250
   * - the player starting in position 0,0
   */
  def initialiseGame1(): Game = {
    /* This method is already implemented. You should not change it */
    return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 100), (3, 3, 250)), 0, 0)
  }

  /**
   * @return A game with
   * - walls in positions 3,3 3,4 3,5 5,3 5,4 and 5,5
   * - a coin at 4,4 which increases score by 200
   * - a coin at 6,3 which increases score by 200
   * - the player starting in position 3,2
   */
  def initialiseGame2(): Game = {
    return new Game(List((3,3),(3,4),(3,5),(5,3),(5,4),(5,5)), List((4,4,200),(6,3,200)), 3, 2)
  }

  /**
   * @return A game with
   * - walls in positions 3,0 3,1 and 3,2
   * - a coin at 4,1 which increases score by 300
   * - a coin at 3,3 which increases score by 150
   * - the player starting in position 4,1
   */
  def initialiseGame3(): Game = {
    return new Game(List((3,0),(3,1),(3,2)), List((4,1,300),(3,3,150)), 4, 1)
  }
}