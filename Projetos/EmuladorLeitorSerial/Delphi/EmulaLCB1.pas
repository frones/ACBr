{$I ACBr.inc}

unit EmulaLCB1;

interface

uses
  synaser, ACBrUtil, 
  SysUtils,
 {$IFDEF Delphi6_UP} Types, {$ELSE} Windows,{$ENDIF}
  Classes, Forms, StdCtrls, Controls ;

type
  TfrEmulador = class(TForm)
    Label1: TLabel;
    edBarra: TEdit;
    Label4: TLabel;
    cbxPorta: TComboBox;
    Label2: TLabel;
    Label5: TLabel;
    edSufixo: TEdit;
    Button1: TButton;
    cbMudaCodigo: TCheckBox;
    Label3: TLabel;
    lEnviado: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
  private
    { Private declarations }
    fSerial : TBlockSerial ;
    Function Converte( cmd : String) : String;
  public
    { Public declarations }
  end;

var
  frEmulador: TfrEmulador;

implementation
Uses ACBrLCB ;

{$R *.dfm}

function TfrEmulador.Converte(cmd: String): String;
var A : Integer ;
begin
  Result := '' ;
  For A := 1 to length( cmd ) do
  begin
     if not (cmd[A] in ['A'..'Z','a'..'z','0'..'9',
                        ' ','.',',','/','?','<','>',';',':',']','[','{','}',
                        '\','|','=','+','-','_',')','(','*','&','^','%','$',
                        '#','@','!','~',']' ]) then
        Result := Result + '#' + IntToStr(ord( cmd[A] )) + ' '
     else
        Result := Result + cmd[A] + ' ';
  end ;
end;

procedure TfrEmulador.FormCreate(Sender: TObject);
begin
  fSerial := TBlockSerial.Create ;
  fSerial.RaiseExcept := true ;
end;

procedure TfrEmulador.Button1Click(Sender: TObject);
begin
  if not fSerial.InstanceActive then
  begin
     fSerial.Connect( cbxPorta.Text );
     fSerial.Config(9600,8,'N',0,false,false);
  end ;

  fSerial.DeadlockTimeout := 1000 ;
  fSerial.Purge ;                   { Limpa a Porta }

  fSerial.SendString( edBarra.Text + TraduzComando( edSufixo.Text ) );

  lEnviado.Caption := Converte( edBarra.Text + TraduzComando( edSufixo.Text ) ) ;
  if cbMudaCodigo.Checked then
  begin
     edBarra.Text := IntToStr( StrToInt64(copy(edBarra.Text,1,12))+1 ) ;
     edBarra.Text := edBarra.Text + EAN13_DV(edBarra.Text) ;
  end ;
end;

procedure TfrEmulador.cbxPortaChange(Sender: TObject);
begin
  if Assigned( fSerial ) then
     if fSerial.InstanceActive then
        fSerial.CloseSocket ;
end;

end.
