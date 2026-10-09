package runner.bots

import runner.*

class ReflexBot(seed: Long = 202L) extends CyberBot:
  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val currentLane = observation.hero.lane

    // маневри в сторони
    def dodge: Action = currentLane match
      case Lane.Left   => Action.MoveRight
      case Lane.Right  => Action.MoveLeft
      case Lane.Center => Action.MoveLeft

    observation.upcoming.headOption match
      case None => Action.KeepRunning
      case Some(slice) =>
        val hasLow = slice.hasObstacleAt(currentLane, Height.Low)
        val hasMid = slice.hasObstacleAt(currentLane, Height.Mid)
        val hasHigh = slice.hasObstacleAt(currentLane, Height.High)

        // 1. Комбо 0+2 обов.язковий маневр
        if hasLow && hasHigh then
          dodge
        // 2. Висота 0 стрибаємо
        else if hasLow then
          Action.Jump
        // 3. Висота 1 присідаємо
        else if hasMid then
          Action.Duck
        // 4. Висота 2 просто біжимо
        else
          Action.KeepRunning