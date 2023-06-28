unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  ACBr.Android.Info, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    ACBrAndroidInfo1: TACBrAndroidInfo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
  procedure AddInfoToMemo(const Titulo, Valor: String);
  begin
      Memo1.Lines.Add(Titulo+': '+Valor)
  end;
begin
  Memo1.Lines.Clear;

  AddInfoToMemo('MANUFACTURER',ACBrAndroidInfo1.MANUFACTURER);
  AddInfoToMemo('MODEL',ACBrAndroidInfo1.MODEL);
  AddInfoToMemo('DEVICE',ACBrAndroidInfo1.DEVICE);
  AddInfoToMemo('PRODUCT',ACBrAndroidInfo1.PRODUCT);
  AddInfoToMemo('BOARD',ACBrAndroidInfo1.BOARD);
  AddInfoToMemo('DISPLAY',ACBrAndroidInfo1.DISPLAY);
  AddInfoToMemo('USER',ACBrAndroidInfo1.USER);
  AddInfoToMemo('FINGERPRINT',ACBrAndroidInfo1.FINGERPRINT);
  AddInfoToMemo('RADIO',ACBrAndroidInfo1.RADIO);
  AddInfoToMemo('HOST',ACBrAndroidInfo1.HOST);
  AddInfoToMemo('ID',ACBrAndroidInfo1.ID);
  AddInfoToMemo('TAGS',ACBrAndroidInfo1.TAGS);
  AddInfoToMemo('BASE_OS',ACBrAndroidInfo1.BASE_OS);
  AddInfoToMemo('CODENAME',ACBrAndroidInfo1.CODENAME);
  AddInfoToMemo('INCREMENTAL',ACBrAndroidInfo1.INCREMENTAL);
  AddInfoToMemo('PREVIEW_SDK_INT',ACBrAndroidInfo1.PREVIEW_SDK_INT.ToString);
  AddInfoToMemo('RELEASE',ACBrAndroidInfo1.RELEASE);
  AddInfoToMemo('SDK',ACBrAndroidInfo1.SDK);
  AddInfoToMemo('SDK_INT',ACBrAndroidInfo1.SDK_INT.ToString);
  AddInfoToMemo('SECURITY_PATCH',ACBrAndroidInfo1.SECURITY_PATCH);
  AddInfoToMemo('SERIAL',ACBrAndroidInfo1.SERIAL);
  Memo1.Lines.Add('');
  AddInfoToMemo('SecondDisplays',ACBrAndroidInfo1.SecondDisplay.ToString);
  AddInfoToMemo('HasSecondDisplay',ACBrAndroidInfo1.HasSecondDisplay.ToString());
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Portas TTY: ');
  Memo1.Lines.AddStrings(ACBrAndroidInfo1.TTY);
end;

end.
