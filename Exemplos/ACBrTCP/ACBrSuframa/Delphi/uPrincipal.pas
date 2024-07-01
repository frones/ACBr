unit uPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrSocket, ACBrSuframa;

type
  TfrmPrincipal = class(TForm)
    ACBrSuframa1: TACBrSuframa;
    edtSuframa: TEdit;
    btnConsultar: TButton;
    Label1: TLabel;
    memResposta: TMemo;
    edtCnpj: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ACBrUtil.Base;

{$R *.dfm}

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  memResposta.Clear;
  edtSuframa.Clear;
  edtCnpj.Clear;
end;

procedure TfrmPrincipal.btnConsultarClick(Sender: TObject);

  procedure AddXMLResposta;
  begin
    if NaoEstaVazio(ACBrSuframa1.HTTPResponse) then
    begin
      memResposta.Lines.Add('');
      memResposta.Lines.Add('XML Resposta:');
      memResposta.Lines.Add(StringOfChar('-', 30));
      memResposta.Lines.Add(ACBrSuframa1.HTTPResponse);
    end;
  end;

begin
  try
    ACBrSuframa1.ConsultarSituacao(
      AnsiString(edtSuframa.Text),
      AnsiString(edtCnpj.Text)
    );

    memResposta.Clear;
    memResposta.Lines.Add('Situação:');
    memResposta.Lines.Add(StringOfChar('-', 30));
    memResposta.Lines.Add(Format('%d - %s', [ACBrSuframa1.Situacao.Codigo, ACBrSuframa1.Situacao.Descricao]));
    AddXMLResposta;
  except
    on E: Exception do
    begin
      memResposta.Clear;
      memResposta.Lines.Add('ERRO:');
      memResposta.Lines.Add(E.Message);
      AddXMLResposta;
    end;
  end;
end;

end.
