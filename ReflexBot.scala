package runner.bots

import runner.*

class ReflexBot(seed: Long = 202L) extends CyberBot:
  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val currentLane = observation.hero.lane

    observation.upcoming.headOption match
      case None => Action.KeepRunning
      case Some(slice) =>
        val hasLow  = slice.hasObstacleAt(currentLane, Height.Low)
        val hasMid  = slice.hasObstacleAt(currentLane, Height.Mid)
        val hasHigh = slice.hasObstacleAt(currentLane, Height.High)

        // перевірка, чи безпечно бігти по смузі
        def isSafeToRun(lane: Lane): Boolean =
          !slice.hasObstacleAt(lane, Height.Low) && !slice.hasObstacleAt(lane, Height.Mid)

        // безпечний маневр убік
        def findSafeSideMove: Action = currentLane match
          case Lane.Left if isSafeToRun(Lane.Center)  => Action.MoveRight
          case Lane.Right if isSafeToRun(Lane.Center) => Action.MoveLeft
          case Lane.Center =>
            if isSafeToRun(Lane.Left) then Action.MoveLeft
            else if isSafeToRun(Lane.Right) then Action.MoveRight
            else Action.KeepRunning
          case _ => Action.KeepRunning

        // повна стіна, комбо Low + High
        if (hasLow && hasHigh) || (hasLow && hasMid) then
          findSafeSideMove

        // низька перешкода - стрибаємо
        else if hasLow then
          Action.Jump

        // середня перешкода - присідаємо
        else if hasMid then
          Action.Duck

        // висока смуга - біжимо
        else
          Action.KeepRunning