package view

import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.Scene
import javafx.scene.control.Alert
import javafx.scene.layout.BorderPane
import javafx.stage.Stage

/**
  * Created by Henrik on 7/9/2016.
  */
class Main extends Application {

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("XMaths")
    val resource = getClass.getResource("/main.fxml")
    println(resource)
    val root: BorderPane = FXMLLoader.load(resource)
    primaryStage.setScene(new Scene(root, 600, 400))
    primaryStage.show()
  }
}

object Main {
  def main(args: Array[String]) {
    Application.launch(classOf[Main], args: _*)
  }

  def error(msg: String, s: String): Unit = {
    val alert = new Alert(Alert.AlertType.ERROR)
    alert.setTitle("Error!")
    alert.setHeaderText(msg)
    alert.setContentText(s)
    alert.showAndWait()
  }
}
