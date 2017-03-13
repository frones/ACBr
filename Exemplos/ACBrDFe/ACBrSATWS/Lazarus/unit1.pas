unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ACBrSATWS, dateutils, ACBrDFeSSL;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrSATWS1: TACBrSATWS;
    btConsultar: TButton;
    eddInicial: TDateEdit;
    eddFinal: TDateEdit;
    ednserieSAT: TEdit;
    edchaveSeguranca: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edhInicial: TTimeEdit;
    edhFinal: TTimeEdit;
    trvwNFe: TTreeView;
    procedure btConsultarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btConsultarClick(Sender: TObject);
var
  I, J : Integer;
  Lote, Node: TTreeNode;
begin
  trvwNFe.Items.Clear;

  ACBrSATWS1.Configuracoes.Geral.SSLLib := libOpenSSL;
  ACBrSATWS1.Configuracoes.WebServices.UF := 'SP';
  //ACBrSATWS1.Configuracoes.WebServices.ProxyHost := '192.168.92.1';
  //ACBrSATWS1.Configuracoes.WebServices.ProxyPort := '3128';

  ACBrSATWS1.WebServices.ConsultarSATWS.versaoDados := '0.07';
  ACBrSATWS1.WebServices.ConsultarSATWS.nserieSAT := StrToIntDef(ednserieSAT.Text,0);
  ACBrSATWS1.WebServices.ConsultarSATWS.dhInicial := eddInicial.Date + TimeOf(edhInicial.Time);
  ACBrSATWS1.WebServices.ConsultarSATWS.dhFinal := eddFinal.Date + TimeOf(edhFinal.Time);
  ACBrSATWS1.WebServices.ConsultarSATWS.chaveSeguranca := edchaveSeguranca.Text;
  if ACBrSATWS1.WebServices.ConsultarSATWS.Executar then
  begin
    trvwNFe.AutoExpand := True;

    if ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote.Count <= 0 then
    begin
      ShowMessage(ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Mensagem);
      exit;
    end;

    for I:=0 to ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote.Count-1 do
    begin
      with ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote[I] do
      begin
        Lote := trvwNFe.Items.Add(nil,NRec);
        trvwNFe.Items.AddChild(Lote,'NRec= ' +NRec);
        trvwNFe.Items.AddChild(Lote,'dhEnvioLote= ' +DateTimeToStr(dhEnvioLote));
        trvwNFe.Items.AddChild(Lote,'dhProcessamento= ' +DateTimeToStr(dhProcessamento));
        trvwNFe.Items.AddChild(Lote,'TipoLote= ' +TipoLote);
        trvwNFe.Items.AddChild(Lote,'Origem= ' +Origem);
        trvwNFe.Items.AddChild(Lote,'QtdeCupoms= ' +IntToStr(QtdeCupoms));
        trvwNFe.Items.AddChild(Lote,'SituacaoLote= ' +SituacaoLote);
        for J:=0 to InfCFe.Count - 1 do
        begin
          Node := trvwNFe.Items.AddChild(Lote,'CFe');
          trvwNFe.Items.AddChild(Node,'Chave= '  +InfCFe[J].Chave);
          trvwNFe.Items.AddChild(Node,'nCupom= '  +InfCFe[J].nCupom);
          trvwNFe.Items.AddChild(Node,'Situacao= '  +InfCFe[J].Situacao);
          trvwNFe.Items.AddChild(Node,'Erros= '  +InfCFe[J].Erros);
        end;
      end;
    end;
  end
  else
    ShowMessage(ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Mensagem);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  eddInicial.Date := IncDay(Now,-10);
  edhInicial.Time := Now;
  eddFinal.Date := Now;
  edhFinal.Time := Now;
end;

end.

