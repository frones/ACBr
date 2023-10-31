  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
  {																			   }
  {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
  { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
  {                                                                              }
  {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
  { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
  { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
  { qualquer versão posterior.                                                   }
  {                                                                              }
  {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
  { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
  { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
  { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
  {                                                                              }
  {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
  { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
  { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
  { Você também pode obter uma copia da licença em:                              }
  { http://www.opensource.org/licenses/lgpl-license.php                          }
  {                                                                              }
  { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
  {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
  {******************************************************************************}
unit frmCalculoSedex;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Mask,
  ACBrBase,
  ACBrSocket,
  ACBrSedex,
  TypInfo,
  ExtCtrls,
  DB,
  DBClient,
  Grids,
  DBGrids,
  ComCtrls,
  TabNotBk,
  DBCtrls;

type
  TForm1 = class(TForm)
    ACBrSedex1: TACBrSedex;
    TabbedNotebook1: TTabbedNotebook;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Image1: TImage;
    Label19: TLabel;
    EditCEPOrigem: TEdit;
    EditCEPDestino: TEdit;
    EditPeso: TEdit;
    EditComprimento: TEdit;
    EditLargura: TEdit;
    EditAltura: TEdit;
    EditDiametro: TEdit;
    cbFormato: TComboBox;
    cbMaoPropria: TComboBox;
    cbAvisoReceb: TComboBox;
    btnConsultar: TButton;
    cbServico: TComboBox;
    EditValorDeclarado: TEdit;
    Panel2: TPanel;
    Label9: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    lblValorAvisoReceb: TLabel;
    Label18: TLabel;
    retValorFrete: TEdit;
    retValorMaoPropria: TEdit;
    retPrzEntrega: TEdit;
    retEntregaSabado: TEdit;
    retEntregaDomiciliar: TEdit;
    retValorAvisoReceb: TEdit;
    retValorDeclarado: TEdit;
    retCodigoServico: TEdit;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Data: TDateTimeField;
    ClientDataSet1Local: TStringField;
    ClientDataSet1Observacao: TStringField;
    EdtRastreio: TEdit;
    Button1: TButton;
    ClientDataSet1Situacao: TStringField;
    Panel3: TPanel;
    DBMemo2: TDBMemo;
    DBMemo1: TDBMemo;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    retDataMaxEntrega: TEdit;
    Panel4: TPanel;
    Label20: TLabel;
    EdtContrato: TEdit;
    Label21: TLabel;
    EdtSenha: TEdit;
    procedure btnConsultarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnConsultarClick(Sender: TObject);
begin
  ACBrSedex1.CodContrato      := EdtContrato.Text;
  ACBrSedex1.Senha            := EdtSenha.Text;
  ACBrSedex1.CepOrigem        := EditCEPOrigem.Text;
  ACBrSedex1.CepDestino       := EditCEPDestino.Text;
  ACBrSedex1.Peso             := StrToFloatDef(EditPeso.Text, 0);
  ACBrSedex1.Formato          := TACBrTpFormato(cbFormato.ItemIndex);
  ACBrSedex1.MaoPropria       := (cbMaoPropria.ItemIndex = 0);
  ACBrSedex1.AvisoRecebimento := (cbAvisoReceb.ItemIndex = 0);
  ACBrSedex1.Comprimento      := StrToFloatDef(EditComprimento.Text, 0);
  ACBrSedex1.Largura          := StrToFloatDef(EditLargura.Text, 0);
  ACBrSedex1.Altura           := StrToFloatDef(EditAltura.Text, 0);
  ACBrSedex1.Servico          := TACBrTpServico(cbServico.ItemIndex);
  ACBrSedex1.Diametro         := StrToFloatDef(EditDiametro.Text, 0);
  ACBrSedex1.ValorDeclarado   := StrToFloatDef(EditValorDeclarado.Text, 0);

  if Not ACBrSedex1.Consultar then
    MessageDlg('Não Foi Possivel Fazer a Consulta:' + sLineBreak + IntToStr(ACBrSedex1.retErro) + ' - ' + ACBrSedex1.retMsgErro, mtError, [ mbOK ], 0)
  Else
  Begin
    retCodigoServico.Text     := ACBrSedex1.retCodigoServico;
    retValorFrete.Text        := FloatToStr(ACBrSedex1.retValor);
    retValorMaoPropria.Text   := FloatToStr(ACBrSedex1.retValorMaoPropria);
    retValorAvisoReceb.Text   := FloatToStr(ACBrSedex1.retValorAvisoRecebimento);
    retValorDeclarado.Text    := FloatToStr(ACBrSedex1.retValorValorDeclarado);
    retEntregaDomiciliar.Text := ACBrSedex1.retEntregaDomiciliar;
    retEntregaSabado.Text     := ACBrSedex1.retEntregaSabado;
    retPrzEntrega.Text        := IntToStr(ACBrSedex1.retPrazoEntrega);
    retDataMaxEntrega.Text    := ACBrSedex1.retDataMaxEntrega;
  End;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var
  I: Integer;
begin
  ACBrSedex1.Rastrear(EdtRastreio.Text);

  Try
    ClientDataSet1.CreateDataSet;
  except
  End;

  For I := 0 to ACBrSedex1.retRastreio.Count - 1 Do
  Begin
    ClientDataSet1.Append;

    ClientDataSet1Data.Value       := ACBrSedex1.retRastreio[ I ].DataHora;
    ClientDataSet1Local.Value      := ACBrSedex1.retRastreio[ I ].Local;
    ClientDataSet1Situacao.Value   := ACBrSedex1.retRastreio[ I ].Situacao;
    ClientDataSet1Observacao.Value := ACBrSedex1.retRastreio[ I ].Observacao;

    ClientDataSet1.Post;
  End;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TabbedNotebook1.PageIndex := 0;
end;

end.
