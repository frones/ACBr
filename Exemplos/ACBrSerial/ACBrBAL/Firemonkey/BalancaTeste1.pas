unit BalancaTeste1;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, ACBrBase, ACBrBAL, FMX.ScrollBox, FMX.Controls.Presentation;

//**   Original VCL Uses section : 


//**   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
//**   Buttons, ExtCtrls, ACBrDevice, ACBrBAL, ACBrBase;


type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrBAL1: TACBrBAL;
    btnConectar: TButton;
    btnDesconectar: TButton;
    btnLerPeso: TButton;
    edLog: TEdit;
    Label12: TLabel;
    SbArqLog: TSpeedButton;
    sttPeso: TLabel;
    sttResposta: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtTimeOut: TEdit;
    Label9: TLabel;
    chbMonitorar: TCheckBox;
    Label10: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    cmbBalanca: TComboBox;
    cmbPortaSerial: TComboBox;
    cmbBaudRate: TComboBox;
    cmbDataBits: TComboBox;
    cmbHandShaking: TComboBox;
    cmbParity: TComboBox;
    Label11: TLabel;
    cmbStopBits: TComboBox;
    procedure btnConectarClick(Sender: TObject);
    procedure btnLerPesoClick(Sender: TObject);
    procedure btnDesconectarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure chbMonitorarClick(Sender: TObject);
    procedure FormCreate(Sender : TObject) ;
    procedure SbArqLogClick(Sender: TObject);
    procedure edtTimeOutKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure ACBrBAL1LePeso(Peso: Double; Resposta: AnsiString);
  private
    { private declarations }
    Function Converte( cmd : String) : String;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.FMX}

Uses
  typinfo,
  ACBrUtil, ACBrDeviceSerial;

function TForm1.Converte(cmd: String): String;
var A : Integer ;
begin
  Result := '' ;
  For A := 1 to length( cmd ) do
  begin
     if not (cmd[A] in ['A'..'Z','a'..'z','0'..'9',
                        ' ','.',',','/','?','<','>',';',':',']','[','{','}',
                        '\','|','=','+','-','_',')','(','*','&','^','%','$',
                        '#','@','!','~' ]) then
        Result := Result + '#' + IntToStr(ord( cmd[A] )) + ' '
     else
        Result := Result + cmd[A] + ' ';
  end ;
end;

procedure TForm1.edtTimeOutKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not (KeyChar in ['0'..'9',#13,#8]) then
     KeyChar := #0 ;
end;

procedure TForm1.ACBrBAL1LePeso(Peso: Double; Resposta: AnsiString);
var
  valid : integer;
begin
   sttPeso.Text      := formatFloat('##0.000', Peso );
   sttResposta.Text  := Converte( Resposta ) ;

   if Peso > 0 then
      Memo1.Lines.Text := 'Leitura OK !'
   else
    begin
      valid := Trunc(ACBrBAL1.UltimoPesoLido);
      case valid of
         0 : Memo1.Lines.Text := 'TimeOut !'+sLineBreak+
                                 'Coloque o produto sobre a Balança!' ;
        -1 : Memo1.Lines.Text := 'Peso Instavel ! ' +sLineBreak+
                                 'Tente Nova Leitura' ;
        -2 : Memo1.Lines.Text := 'Peso Negativo !' ;
       -10 : Memo1.Lines.Text := 'Sobrepeso !' ;
      end;
    end ;
end;

procedure TForm1.btnConectarClick(Sender: TObject);
begin
   // se houver conecção aberta, Fecha a conecção
   if acbrBal1.Ativo then
      ACBrBAL1.Desativar;

   // configura porta de comunicação
   ACBrBAL1.Modelo           := TACBrBALModelo( cmbBalanca.ItemIndex );
   ACBrBAL1.Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex );
   ACBrBAL1.Device.Parity    := TACBrSerialParity( cmbParity.ItemIndex );
   ACBrBAL1.Device.Stop      := TACBrSerialStop( cmbStopBits.ItemIndex );
   ACBrBAL1.Device.Data      := StrToInt( cmbDataBits.Selected.Text );
   ACBrBAL1.Device.Baud      := StrToInt( cmbBaudRate.Selected.Text );
   ACBrBAL1.Device.Porta     := cmbPortaSerial.Selected.Text;
   ACBrBAL1.ArqLOG           := edLog.Text;

   // Conecta com a balança
   ACBrBAL1.Ativar;

   btnConectar.Enabled    := false;
   Panel1.Enabled         := false;
   btnDesconectar.Enabled := true;
   btnLerPeso.Enabled     := true;
end;

procedure TForm1.btnLerPesoClick(Sender: TObject);
Var TimeOut : Integer ;
begin
   try
      TimeOut := StrToInt( edtTimeOut.Text ) ;
   except
      TimeOut := 2000 ;
   end ;

   ACBrBAL1.LePeso( TimeOut );
end;

procedure TForm1.btnDesconectarClick(Sender: TObject);
begin
  ACBrBAL1.Desativar;

  btnConectar.Enabled    := True;
  Panel1.Enabled         := True;
  btnDesconectar.Enabled := False;
  btnLerPeso.Enabled     := False;
end;


procedure TForm1.chbMonitorarClick(Sender: TObject);
begin
   ACBrBAL1.MonitorarBalanca := chbMonitorar.IsChecked  ;
end;

procedure TForm1.FormCreate(Sender : TObject) ;
var
  I : TACBrBALModelo ;
begin
  cmbBalanca.Items.Clear ;
  For I := Low(TACBrBALModelo) to High(TACBrBALModelo) do
     cmbBalanca.Items.Add( GetEnumName(TypeInfo(TACBrBALModelo), integer(I) ) ) ;
  cmbBalanca.ItemIndex := 0;
end;

procedure TForm1.SbArqLogClick(Sender: TObject);
begin
  OpenURL( ExtractFilePath( ParamStr(0) ) + edLog.Text);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ACBrBAL1.Desativar ;
end;

end.

