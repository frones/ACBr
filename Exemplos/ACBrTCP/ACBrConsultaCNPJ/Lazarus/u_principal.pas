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

unit U_Principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCL, LCLType,
  LCLProc, ExtCtrls, StdCtrls, Buttons, MaskEdit, ComCtrls, ACBrConsultaCNPJ,
  ACBrBase, ACBrSocket, TypInfo;


{$DEFINE SUPPORT_PNG}  

{$IFDEF DELPHI2009_UP}
  {$DEFINE SUPPORT_PNG}
{$ENDIF}

type

  { TF_Principal }

  TF_Principal = class(TForm)
    ButBuscar: TBitBtn;
    cbbProvedor: TComboBox;
    edtHost: TEdit;
    edtPort: TEdit;
    edtUsuario: TEdit;
    edtSenha: TEdit;
    EditCNPJ: TMaskEdit;
    Label1: TLabel;
    Label14: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    EditTipo: TEdit;
    EditRazaoSocial: TEdit;
    EditAbertura: TEdit;
    EditEndereco: TEdit;
    EditNumero: TEdit;
    EditComplemento: TEdit;
    EditBairro: TEdit;
    EditCidade: TEdit;
    EditUF: TEdit;
    EditCEP: TEdit;
    EditSituacao: TEdit;
    Panel1: TPanel;
    EditFantasia: TEdit;
    Label13: TLabel;
    ACBrConsultaCNPJ1: TACBrConsultaCNPJ;
    ListCNAE2: TListBox;
    Label15: TLabel;
    EditCNAE1: TEdit;
    Label16: TLabel;
    EditEmail: TEdit;
    Label17: TLabel;
    EditTelefone: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    EditPorte: TEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ButBuscarClick(Sender: TObject);
    procedure EditCaptchaKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Principal: TF_Principal;

implementation

{$R *.lfm}

procedure TF_Principal.FormCreate(Sender: TObject);
var Provedor : TACBrCNPJProvedorWS;
begin
  cbbProvedor.Items.Clear;

  for Provedor := Low(TACBrCNPJProvedorWS) to High(TACBrCNPJProvedorWS) do
    cbbProvedor.Items.AddObject( GetEnumName(TypeInfo(TACBrCNPJProvedorWS), integer(Provedor) ), TObject(integer(Provedor)) );

  cbbProvedor.ItemIndex := 0;
end;

procedure TF_Principal.ButBuscarClick(Sender: TObject);
var
  I: Integer;
begin
  ACBrConsultaCNPJ1.Provedor := TACBrCNPJProvedorWS(cbbProvedor.ItemIndex);
  ACBrConsultaCNPJ1.ProxyHost:= edtHost.Text;
  ACBrConsultaCNPJ1.ProxyPort:= edtPort.Text;
  ACBrConsultaCNPJ1.ProxyUser:= edtUsuario.Text;
  ACBrConsultaCNPJ1.ProxyPass:= edtSenha.Text;
  if ACBrConsultaCNPJ1.Provedor = cwsNenhum then
     raise EACBrConsultaCNPJException.Create('Nenhum provedor Selecionado!');

  if ACBrConsultaCNPJ1.Consulta(EditCNPJ.Text) then
  begin
    EditTipo.Text        := ACBrConsultaCNPJ1.EmpresaTipo;
    EditRazaoSocial.Text := ACBrConsultaCNPJ1.RazaoSocial;
    EditPorte.Text       := ACBrConsultaCNPJ1.Porte;
    EditAbertura.Text    := DateToStr( ACBrConsultaCNPJ1.Abertura );
    EditFantasia.Text    := ACBrConsultaCNPJ1.Fantasia;
    EditEndereco.Text    := ACBrConsultaCNPJ1.Endereco;
    EditNumero.Text      := ACBrConsultaCNPJ1.Numero;
    EditComplemento.Text := ACBrConsultaCNPJ1.Complemento;
    EditBairro.Text      := ACBrConsultaCNPJ1.Bairro;
    EditComplemento.Text := ACBrConsultaCNPJ1.Complemento;
    EditCidade.Text      := ACBrConsultaCNPJ1.Cidade;
    EditUF.Text          := ACBrConsultaCNPJ1.UF;
    EditCEP.Text         := ACBrConsultaCNPJ1.CEP;
    EditSituacao.Text    := ACBrConsultaCNPJ1.Situacao;
    EditCNAE1.Text       := ACBrConsultaCNPJ1.CNAE1;
    EditEmail.Text       := ACBrConsultaCNPJ1.EndEletronico;
    EditTelefone.Text    := ACBrConsultaCNPJ1.Telefone;

    ListCNAE2.Clear;
    for I := 0 to ACBrConsultaCNPJ1.CNAE2.Count - 1 do
      ListCNAE2.Items.Add(ACBrConsultaCNPJ1.CNAE2[I]);
  end;
end;

procedure TF_Principal.EditCaptchaKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ButBuscarClick(ButBuscar);
end;

end.
