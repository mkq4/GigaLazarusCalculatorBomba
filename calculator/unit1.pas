unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormCalculator }

  TFormCalculator = class(TForm)
    BtnEqual: TImage;           // =
    BtnInverse: TImage;         // 1/x
    BtnSquare: TImage;          // x**2
    BtnSqrt: TImage;            // sqrt
    BtnClearEntry: TImage;      // CE
    BtnClearAll: TImage;        // C
    BtnBackspace: TImage;       // <-
    BtnDecimal: TImage;         // ,
    BtnAdd: TImage;             // +
    BtnSubtract: TImage;        // -
    BtnMultiply: TImage;        // *
    BtnDivide: TImage;          // /
    Display: TEdit;             // input
    background: TImage;         // background

    Btn0: TImage;
    Btn1: TImage;
    Btn2: TImage;
    Btn3: TImage;
    Btn4: TImage;
    Btn5: TImage;
    Btn6: TImage;
    Btn7: TImage;
    Btn8: TImage;
    Btn9: TImage;

    procedure ImageDigitClick(Sender: TObject); // chars handler
    procedure OperatorClick(Sender: TObject);   // operators (+ - * / )
    procedure BtnEqualClick(Sender: TObject);   // =
    procedure BtnClearEntryClick(Sender: TObject); // CE
    procedure BtnClearAllClick(Sender: TObject);   // C
    procedure BtnBackspaceClick(Sender: TObject);  // ←
    procedure BtnInverseClick(Sender: TObject);    // 1/x
    procedure BtnSquareClick(Sender: TObject);     // x**2
    procedure BtnDecimalClick(Sender: TObject);    // ,
    procedure BtnSqrtClick(Sender: TObject);       // sqrt
    procedure FormCreate(Sender: TObject);

  private
    Operand1: Double;
    Operand2: Double;
    CurrentOperator: String;
    OperatorClicked: Boolean;
  public
  end;

var
  FormCalculator: TFormCalculator;

implementation

{$R *.lfm}

{ вызывается при нажатии на картинку с цифрой }
procedure TFormCalculator.ImageDigitClick(Sender: TObject);
var
  digit: String;
begin
  // init num for obj
  if Sender = Btn0 then digit := '0'
  else if Sender = Btn1 then digit := '1'
  else if Sender = Btn2 then digit := '2'
  else if Sender = Btn3 then digit := '3'
  else if Sender = Btn4 then digit := '4'
  else if Sender = Btn5 then digit := '5'
  else if Sender = Btn6 then digit := '6'
  else if Sender = Btn7 then digit := '7'
  else if Sender = Btn8 then digit := '8'
  else if Sender = Btn9 then digit := '9'
  else Exit;

  // clear screen if clicked operator
  if OperatorClicked then
  begin
    Display.Text := '';
    OperatorClicked := False;
  end;

  // add num at field
  Display.Text := Display.Text + digit;
end;

{ save operator and last nums }
procedure TFormCalculator.OperatorClick(Sender: TObject);
begin
  if Display.Text <> '' then
  begin
    try
      Operand1 := StrToFloat(Display.Text);
      if Sender = BtnAdd then
        CurrentOperator := '+'
      else if Sender = BtnSubtract then
        CurrentOperator := '-'
      else if Sender = BtnMultiply then
        CurrentOperator := '×'
      else if Sender = BtnDivide then
        CurrentOperator := '÷'
      else
        Exit; // Некорректная кнопка
      OperatorClicked := True;
    except
      on E: Exception do
        ShowMessage('Неверное число');
    end;
  end;
end;

{ = }
procedure TFormCalculator.BtnEqualClick(Sender: TObject);
begin
  if Display.Text <> '' then
  begin
    try
      Operand2 := StrToFloat(Display.Text);
      case CurrentOperator of
        '+': Display.Text := FloatToStr(Operand1 + Operand2);
        '-': Display.Text := FloatToStr(Operand1 - Operand2);
        '×': Display.Text := FloatToStr(Operand1 * Operand2);
        '÷':
          begin
            if Operand2 = 0 then
            begin
              ShowMessage('Деление на ноль запрещено!');
              Exit;
            end;
            Display.Text := FloatToStr(Operand1 / Operand2);
          end;
      end;
      OperatorClicked := True;
    except
      on E: Exception do
        ShowMessage('Ошибка вычисления');
    end;
  end;
end;

{ clear this field }
procedure TFormCalculator.BtnClearEntryClick(Sender: TObject);
begin
  Display.Clear;
end;

{ clear all }
procedure TFormCalculator.BtnClearAllClick(Sender: TObject);
begin
  Display.Clear;
  Operand1 := 0;
  Operand2 := 0;
  CurrentOperator := '';
  OperatorClicked := False;
end;

{ del last char }
procedure TFormCalculator.BtnBackspaceClick(Sender: TObject);
var
  s: String;
begin
  s := Display.Text;
  if Length(s) > 0 then
    Delete(s, Length(s), 1);
  Display.Text := s;
end;

{ 1/x }
procedure TFormCalculator.BtnInverseClick(Sender: TObject);
var
  num: Real;
begin
  if Display.Text = '' then Exit;
  try
    num := StrToFloat(Display.Text);
    if num = 0 then
      raise Exception.Create('На ноль делить нельзя!');
    Display.Text := FloatToStr(1 / num);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{ , }
procedure TFormCalculator.BtnDecimalClick(Sender: TObject);
begin
  if Display.Text = '' then
    Display.Text := '0' + FormatSettings.DecimalSeparator
  else if Pos(FormatSettings.DecimalSeparator, Display.Text) = 0 then
    Display.Text := Display.Text + FormatSettings.DecimalSeparator;
end;

{ squre }
procedure TFormCalculator.BtnSquareClick(Sender: TObject);
var
  num: Real;
begin
  if Display.Text = '' then Exit;
  try
    num := StrToFloat(Display.Text);
    Display.Text := FloatToStr(Sqr(num));
  except
    on E: Exception do
      ShowMessage('Неверное число');
  end;
end;

{ sqrt }
procedure TFormCalculator.BtnSqrtClick(Sender: TObject);
var
  num: Real;
begin
  try
    if Display.Text = '' then Exit;
    num := StrToFloat(Display.Text);
    if num < 0 then
      raise Exception.Create('Корень из отрицательного числа!');
    Display.Text := FloatToStr(Sqrt(num));
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{ INIT }
procedure TFormCalculator.FormCreate(Sender: TObject);
begin
  Operand1 := 0;
  Operand2 := 0;
  CurrentOperator := '';
  OperatorClicked := False;
  Display.Text := '';
  Scaled := False; // no scale
  BorderStyle := bsSingle; // no change w and h by user
end;

end.
