unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormCalculator }

 TFormCalculator = class(TForm)
    BtnEqual: TButton;           // =
    BtnInverse: TButton;         // 1/x
    BtnSquare: TButton;          // x²
    BtnSqrt: TButton;            // √
    BtnClearEntry: TButton;      // CE (очистить текущее)
    BtnClearAll: TButton;        // C (очистить всё)
    BtnBackspace: TButton;       // ← (удалить последний символ)
    BtnDecimal: TButton;         // , или . (дробная точка)
    BtnAdd: TButton;             // +
    BtnSubtract: TButton;        // -
    BtnMultiply: TButton;        // ×
    BtnDivide: TButton;          // ÷
    Btn0: TButton;               // 0
    Btn1: TButton;               // 1
    Btn2: TButton;               // 2
    Btn3: TButton;               // 3
    Btn4: TButton;               // 4
    Btn5: TButton;               // 5
    Btn6: TButton;               // 6
    Btn7: TButton;               // 7
    Btn8: TButton;               // 8
    Btn9: TButton;               // 9
    Display: TEdit;              // поле вывода
    Image1: TImage;


    procedure BtnClick(Sender: TObject);
    procedure OperatorClick(Sender: TObject);
    procedure BtnEqualClick(Sender: TObject);
    procedure BtnClearEntryClick(Sender: TObject);
    procedure BtnClearAllClick(Sender: TObject);
    procedure BtnBackspaceClick(Sender: TObject);
    procedure BtnInverseClick(Sender: TObject);
    procedure BtnSquareClick(Sender: TObject);
    procedure BtnDecimalClick(Sender: TObject);
    procedure BtnSqrtClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
   private
     Operand1: Double;          // Первое число
     Operand2: Double;          // Второе число
     CurrentOperator: String;   // Текущая операция (+, -, *, /)
     OperatorClicked: Boolean;  // Флаг, указывающий, что была нажата операция
  public
    { public declarations }
  end;

var
  FormCalculator: TFormCalculator;
  Operand1, Operand2, ResultValue: Real;
  CurrentOperator: String;
  OperatorClicked: Boolean = False;

implementation

{$R *.lfm}

{ TFormCalculator }

{ добавляет цифру или точку в поле ввода }
procedure TFormCalculator.BtnClick(Sender: TObject);
var
  btnCaption: String;
begin
  btnCaption := (Sender as TButton).Caption;  // Получаем символ с кнопки
  if OperatorClicked then
  begin
    Display.Text := '';        // Очищаем поле ввода
    OperatorClicked := False;  // Сбрасываем флаг
  end;
  Display.Text := Display.Text + btnCaption;  // Добавляем новую цифру
end;

{ сохраняет выбранный оператор и первый операнд }
procedure TFormCalculator.OperatorClick(Sender: TObject);
begin
  if Display.Text <> '' then  // Проверяем, что поле не пустое
  begin
    try
      Operand1 := StrToFloat(Display.Text);  // Сохраняем первое число
      CurrentOperator := (Sender as TButton).Caption;  // Запоминаем операцию
      OperatorClicked := True;  // Устанавливаем флаг
    except
      on E: Exception do
        ShowMessage('Неверное число');
    end;
  end;
end;

{ выполняет вычисление при нажатии "=" }
procedure TFormCalculator.BtnEqualClick(Sender: TObject);
begin
  if Display.Text <> '' then
  begin
    try
      Operand2 := StrToFloat(Display.Text);  // Сохраняем второе число
      case CurrentOperator of
        '+': Display.Text := FloatToStr(Operand1 + Operand2);
        '-': Display.Text := FloatToStr(Operand1 - Operand2);
        '*': Display.Text := FloatToStr(Operand1 * Operand2);
        '/':
          begin
            if Operand2 = 0 then
            begin
              ShowMessage('Деление на ноль запрещено!');
              Exit;
            end;
            Display.Text := FloatToStr(Operand1 / Operand2);
          end;
      end;
      OperatorClicked := True;  // Готовимся к новому вводу
    except
      on E: Exception do
        ShowMessage('Ошибка вычисления');
    end;
  end;
end;

{ очищает текущее поле ввода }
procedure TFormCalculator.BtnClearEntryClick(Sender: TObject);
begin
  Display.Clear;
end;

{ полностью сбрасывает калькулятор }
procedure TFormCalculator.BtnClearAllClick(Sender: TObject);
begin
  Display.Clear;
  Operand1 := 0;
  Operand2 := 0;
  ResultValue := 0;
  CurrentOperator := '';
end;

{ удаляет последний символ }
procedure TFormCalculator.BtnBackspaceClick(Sender: TObject);
var
  s: String;
begin
  s := Display.Text;
  if Length(s) > 0 then
    Delete(s, Length(s), 1);
  Display.Text := s;
end;

{ вычисляет обратное значение числа }
procedure TFormCalculator.BtnInverseClick(Sender: TObject);
var
  num: Real;
begin
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

// oбработчик кнопки десятичного разделителя, предотвращает ввод нескольких разделителей
procedure TFormCalculator.BtnDecimalClick(Sender: TObject);
begin
  if Display.Text = '' then
    Display.Text := '0' + FormatSettings.DecimalSeparator
  else if Pos(FormatSettings.DecimalSeparator, Display.Text) = 0 then
    Display.Text := Display.Text + FormatSettings.DecimalSeparator;
end;

{ возводит число в квадрат }
procedure TFormCalculator.BtnSquareClick(Sender: TObject);
var
  num: Real;
begin
  try
    num := StrToFloat(Display.Text);
    Display.Text := FloatToStr(Sqr(num));
  except
    on E: Exception do
      ShowMessage('Неверное число');
  end;
end;

{ извлекает квадратный корень }
procedure TFormCalculator.BtnSqrtClick(Sender: TObject);
var
  num: Real;
begin
  try
    num := StrToFloat(Display.Text);
    if num < 0 then
      raise Exception.Create('Корень из отрицательного числа!');
    Display.Text := FloatToStr(Sqrt(num));
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{ настраивает интерфейс при запуске формы }

procedure TFormCalculator.FormCreate(Sender: TObject);
begin
  Operand1 := 0;
  Operand2 := 0;
  CurrentOperator := '';
  OperatorClicked := False;
  Display.Text := '';
end;

end.

