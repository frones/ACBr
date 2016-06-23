unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Spin, ComCtrls, DBGrids, ACBrTCP,
  ACBrMTer, ACBrSocket, ACBrConsts, blcksock;

type

  { TForm1 }
  TForm1 = class(TForm)
    ACBrMTer1: TACBrMTer;
    btAtivar: TButton;
    btAtualizar: TButton;
    btBackSpace: TButton;
    btBeep: TButton;
    btDesativar: TButton;
    btDeslocarCursor: TButton;
    btDeslocarLinha: TButton;
    btEnviarParalela: TButton;
    btEnviarSerial: TButton;
    btEnviarTexto: TButton;
    btFluxoVendas: TButton;
    btLimparDisplay: TButton;
    btLimparLinha: TButton;
    btLimparLinha1: TButton;
    btPosicionarCursor: TButton;
    cbEcoAutomatico: TCheckBox;
    cbModelo: TComboBox;
    clbConectados: TCheckListBox;
    dbgComandas: TDBGrid;
    dbgTerminais: TDBGrid;
    dsComandas: TDataSource;
    dsTerminais: TDataSource;
    edDesLinha: TSpinEdit;
    edEnviarParalela: TEdit;
    edEnviarSerial: TEdit;
    edEnviarTexto: TEdit;
    edLimparLinha: TSpinEdit;
    edPorta: TEdit;
    edPosColuna: TSpinEdit;
    edPosLinha: TSpinEdit;
    edQtdPosicao: TSpinEdit;
    edSerial: TSpinEdit;
    edTerminador: TEdit;
    edTimeout: TEdit;
    gbComandas: TGroupBox;
    Label1: TLabel;
    lbDesLinha: TLabel;
    lbLimparLinha: TLabel;
    lbModelo: TLabel;
    lbPorta: TLabel;
    lbPosColuna: TLabel;
    lbPosLinha: TLabel;
    lbQtdPosicoes: TLabel;
    lbSerial: TLabel;
    lbTerminador: TLabel;
    memComandas: TMemDataset;
    memTerminais: TMemDataset;
    mOutput: TMemo;
    PageControl2: TPageControl;
    pnAtivarFluxo: TPanel;
    pnComandas: TPanel;
    pnComandos: TPanel;
    pnConectados: TPanel;
    pnConfig: TPanel;
    pnLegenda: TPanel;
    pnTerminais: TPanel;
    Splitter1: TSplitter;
    tsComandos: TTabSheet;
    tsFluxoVendas: TTabSheet;
    procedure ACBrMTer1Conecta(const IP: AnsiString);
    procedure ACBrMTer1Desconecta(const IP: AnsiString; Erro: Integer;
      ErroDesc: AnsiString);
    procedure ACBrMTer1RecebeDados(const IP: AnsiString;
      const Recebido: AnsiString);
    procedure btAtivarClick(Sender: TObject);
    procedure btAtualizarClick(Sender: TObject);
    procedure btBackSpaceClick(Sender: TObject);
    procedure btBeepClick(Sender: TObject);
    procedure btDesativarClick(Sender: TObject);
    procedure btDeslocarCursorClick(Sender: TObject);
    procedure btDeslocarLinhaClick(Sender: TObject);
    procedure btEnviarParalelaClick(Sender: TObject);
    procedure btEnviarSerialClick(Sender: TObject);
    procedure btEnviarTextoClick(Sender: TObject);
    procedure btLimparDisplayClick(Sender: TObject);
    procedure btLimparLinha1Click(Sender: TObject);
    procedure btLimparLinhaClick(Sender: TObject);
    procedure btPosicionarCursorClick(Sender: TObject);
    procedure btFluxoVendasClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
  private
    procedure AtualizarConexoes;
    procedure VerificaSelecionado;

    procedure CarregarTerminais;
    procedure IniciarFluxoVendas;
    function AlterarEstadoTerminal(aIP: String; aEstado: Integer): Boolean;

    procedure AvaliarRespostaTerminal(aIP, aString: String);
    procedure IncluirComanda(aComanda: String);
    procedure AdicionaItem(aComanda: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  strutils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btAtivarClick(Sender: TObject);
begin
  with ACBrMTer1 do
  begin
    Modelo     := TACBrMTerModelo(cbModelo.ItemIndex);
    Port       := edPorta.Text;
    Terminador := edTerminador.Text;
    TimeOut    := StrToInt(edTimeout.Text);
    EcoAuto    := cbEcoAutomatico.Checked;
    Ativar;
  end;
  mOutput.Lines.Add('Escutando porta: ' + edPorta.Text);
end;

procedure TForm1.ACBrMTer1Conecta(const IP: AnsiString);
begin
  mOutput.Lines.Add('Conectou IP: ' + IP);

  ACBrMTer1.LimparDisplay(IP);
  ACBrMTer1.EnviarTexto(IP, 'Seja bem vindo');

  AtualizarConexoes;
end;

procedure TForm1.ACBrMTer1Desconecta(const IP: AnsiString; Erro: Integer;
  ErroDesc: AnsiString);
begin
  mOutput.Lines.Add('Desconectou IP: ' + IP);
  mOutput.Lines.Add('  - Erro: ' + IntToStr(Erro) + ' - ' + ErroDesc);

  AtualizarConexoes;
end;

procedure TForm1.ACBrMTer1RecebeDados(const IP: AnsiString;
  const Recebido: AnsiString);
begin
  mOutput.Lines.Add('IP: ' + IP + ' - Recebido :' + Recebido);

  if (PageControl2.ActivePageIndex = 1) then
    AvaliarRespostaTerminal(IP, Recebido);
end;

procedure TForm1.btAtualizarClick(Sender: TObject);
begin
  AtualizarConexoes;
end;

procedure TForm1.btBackSpaceClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.BackSpace(clbConectados.Items[I]);  // Envia Comando usando IP
end;

procedure TForm1.btBeepClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.Beep(clbConectados.Items[I]);
end;

procedure TForm1.btDesativarClick(Sender: TObject);
begin
  ACBrMTer1.Desativar;
  mOutput.Lines.Add('Desativada porta: ' + edPorta.Text);
end;

procedure TForm1.btDeslocarCursorClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.DeslocarCursor(clbConectados.Items[I], edQtdPosicao.Value);
end;

procedure TForm1.btDeslocarLinhaClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.DeslocarLinha(clbConectados.Items[I], edDesLinha.Value);
end;

procedure TForm1.btEnviarParalelaClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.EnviarParaParalela(clbConectados.Items[I], edEnviarParalela.Text);
end;

procedure TForm1.btEnviarSerialClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.EnviarParaSerial(clbConectados.Items[I], edEnviarSerial.Text, edSerial.Value);
end;

procedure TForm1.btEnviarTextoClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.EnviarTexto(clbConectados.Items[I], edEnviarTexto.Text);
end;

procedure TForm1.btLimparDisplayClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.LimparDisplay(clbConectados.Items[I]);
end;

procedure TForm1.btLimparLinha1Click(Sender: TObject);
var
  IsOnLine: Boolean;
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
    begin
      IsOnLine := ACBrMTer1.Online(clbConectados.Items[I]);
      mOutput.Lines.Add('Terminal: '+clbConectados.Items[I]+' - '+IfThen(IsOnLine,'On Line','Off Line') );
    end;
end;

procedure TForm1.btLimparLinhaClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.LimparLinha(clbConectados.Items[I], edLimparLinha.Value);
end;

procedure TForm1.btPosicionarCursorClick(Sender: TObject);
var
  I: Integer;
begin
  VerificaSelecionado;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      ACBrMTer1.PosicionarCursor(clbConectados.Items[I], edPosLinha.Value, edPosColuna.Value);
end;

procedure TForm1.btFluxoVendasClick(Sender: TObject);
begin
  IniciarFluxoVendas;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl2.ActivePageIndex := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ACBrMTer1.Desativar;
end;

procedure TForm1.PageControl2Change(Sender: TObject);
begin
  if (PageControl2.PageIndex = 1) then
    CarregarTerminais;

  clbConectados.Visible := (PageControl2.PageIndex = 0);
  Splitter1.Visible     := (clbConectados.Visible);
end;

procedure TForm1.AtualizarConexoes;
var
  I: Integer;
begin
  clbConectados.Clear;

  with ACBrMTer1.TCPServer.ThreadList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      with TACBrTCPServerThread(Items[I]) do
        if Active then
          clbConectados.Items.Add(TCPBlockSocket.GetRemoteSinIP);
    end;
  finally
    ACBrMTer1.TCPServer.ThreadList.UnlockList;
  end;

  Application.ProcessMessages;
end;

procedure TForm1.VerificaSelecionado;
var
  wSel: Boolean;
  I: Integer;
begin
  wSel := False;

  for I := 0 to clbConectados.Count - 1 do
    if clbConectados.Checked[I] then
      wSel := True;

  if (not wSel) then
    ShowMessage('Selecione um Terminal');
end;

procedure TForm1.CarregarTerminais;
var
  I: Integer;
begin
  memComandas.Clear(False);
  memComandas.Open;
  memTerminais.Clear(False);
  memTerminais.Open;

  with ACBrMTer1.TCPServer.ThreadList.LockList do
  try
    for I := 0 to Count - 1 do
      with TACBrTCPServerThread(Items[I]) do
        if Active then
        begin
          memTerminais.Insert;
          memTerminais.FieldByName('IP_TERMINAL').AsString := TCPBlockSocket.GetRemoteSinIP;
          memTerminais.FieldByName('COMANDA').AsString     := '';
          memTerminais.FieldByName('RESPOSTA').AsString    := '';
          memTerminais.FieldByName('STATUS').AsInteger     :=  0;
          memTerminais.Post;
        end;
  finally
    ACBrMTer1.TCPServer.ThreadList.UnlockList;
  end;

  Application.ProcessMessages;
end;

procedure TForm1.IniciarFluxoVendas;
begin
  if memTerminais.IsEmpty then
  begin
    ShowMessage('Nenhum terminal disponível');
    Exit;
  end;

  with memTerminais do
  begin
    DisableControls;
    try
      First;
      while (not EOF) do
      begin
        Edit;
        if AlterarEstadoTerminal(Fields[0].AsString, 1) then
          FieldByName('STATUS').AsInteger := 1
        else
          FieldByName('STATUS').AsInteger := -1;
        Post;

        Next;
      end;
    finally
      EnableControls;
    end;
  end;
end;

function TForm1.AlterarEstadoTerminal(aIP: String; aEstado: Integer): Boolean;
begin
  Result := True;

  try
    ACBrMTer1.LimparDisplay(aIP);

    case aEstado of
      0: Exit;                                               // Indefinido
      1: ACBrMTer1.EnviarTexto(aIP, 'Informe a Comanda: ');  // Pede Comanda
      2: ACBrMTer1.EnviarTexto(aIP, 'Informe o Item: ');     // Pede Item
    end;
  except
    Result := False;
  end;
end;

procedure TForm1.AvaliarRespostaTerminal(aIP, aString: String);
begin
  with memTerminais do
  begin
    DisableControls;
    try
      if Locate('IP_TERMINAL', aIP, []) then
      begin
        if (aString[1] <> #13) then
        begin
          // Grava Resposta
          Edit;
          FieldByName('RESPOSTA').AsString := FieldByName('RESPOSTA').AsString + aString;
          Post;

          Exit;
        end;

        case FieldByName('STATUS').AsInteger of
          1:
          begin
            Edit;
            FieldByName('COMANDA').AsString  := FieldByName('RESPOSTA').AsString;
            FieldByName('RESPOSTA').AsString := '';

            if AlterarEstadoTerminal(aIP, 2) then  // Muda estado para pedir Item
              FieldByName('STATUS').AsInteger := 2
            else
              FieldByName('STATUS').AsInteger := -1;
            Post;

            IncluirComanda(FieldByName('COMANDA').AsString);
          end;

          2:
          begin
            AdicionaItem(FieldByName('COMANDA').AsString);

            Edit;
            FieldByName('COMANDA').AsString  := '';
            FieldByName('RESPOSTA').AsString := '';

            if AlterarEstadoTerminal(aIP, 1) then
              FieldByName('STATUS').AsInteger := 1
            else
              FieldByName('STATUS').AsInteger := -1;
            Post;
          end;
        end;
      end;
    finally
      EnableControls;
    end;
  end;
end;

procedure TForm1.IncluirComanda(aComanda: String);
begin
  with memComandas do
  begin
    DisableControls;
    try
      First;
      while (not EOF) do
      begin
        if (FieldByName('CODCOMANDA').AsString = aComanda) then
          Exit;   // Comanda já existe? ...Sai

        Next;
      end;

      Insert;
      FieldByName('CODCOMANDA').AsString := aComanda;
      FieldByName('QTD_ITENS').AsInteger := 0;
      Post;
    finally
      EnableControls;
    end;
  end;
end;

procedure TForm1.AdicionaItem(aComanda: String);
begin
  with memComandas do
  begin
    DisableControls;
    try
      First;
      while (not EOF) do
      begin
        if (FieldByName('CODCOMANDA').AsString = aComanda) then
        begin
          Edit;
          FieldByName('QTD_ITENS').AsInteger := FieldByName('QTD_ITENS').AsInteger + 1;
          Post;

          Exit;
        end;

        Next;
      end;
    finally
      EnableControls;
    end;
  end;
end;

end.

