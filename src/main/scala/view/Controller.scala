package view

import javafx.fxml.FXML
import javafx.scene.control.{Label, TextField}

import func.{FuncUtils, Function, FunctionParser}
import func.FuncUtils.MathString

/**
  * Created by Henrik on 7/9/2016.
  */
class Controller {
  @FXML
  var functionLabel: Label = _
  @FXML
  var derivLabel: Label = _
  @FXML
  var integralLabel: Label = _
  @FXML
  var checkLabel: Label = _
  @FXML
  var valueLabel: Label = _
  @FXML
  var functionField: TextField = _
  @FXML
  var valueField: TextField = _

  var function: Function = _

  @FXML
  def checkFunction(): Unit = {
    try {
      function = FunctionParser.parse(functionField.getText)
      checkLabel.setText("Function ok!")
      functionLabel.setText("f(x) = " + function)
      derivLabel.setText("f'(x) = " + function.derive())
      integralLabel.setText("F(x) = " + function.antiderive() + " + C")
      if (!valueField.getText.isEmpty) {
        evaluate()
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Main.error("Invalid Function", e.toString + ": " + functionField.getText)
        checkLabel.setText("Function invalid!")
    }
  }

  @FXML
  def evaluate(): Unit = {
    try {
      val x = FunctionParser.evaluate(valueField.getText)
      val y = function.get(x)
      valueLabel.setText("f(" + x.toMathString + ") = " + y.toMathString)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Main.error("Invalid x-value!", e.getMessage)
        valueLabel.setText("Enter a value.")
    }
  }

}
