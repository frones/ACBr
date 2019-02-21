{******************************************************************************}
{ Projeto: Demo para impressão de DANFSe em Fast-Report                        }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
******************************************************************************}
unit ufrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.IOUtils,pcnConversao,
  ACBrBase, ACBrDFe, frxClass, Vcl.ComCtrls, ACBrNFSeDANFSeClass, ACBrNFSeDANFSeFR, ACBrNFSe,
  ACBrDFeReport;

type
  TfrmPrincipal = class(TForm)
    imgLogo: TImage;
    pnlbotoes: TPanel;
    btnImprimir: TButton;
    btncarregar: TButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    frxReport1: TfrxReport;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lstbxFR3: TListBox;
    TabSheet2: TTabSheet;
    RbCanhoto: TRadioGroup;
    ACBrNFSe1: TACBrNFSe;
    ACBrNFSeDANFSeFR1: TACBrNFSeDANFSeFR;
    procedure FormCreate(Sender: TObject);
    procedure btncarregarClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
  private
    procedure Configuracao;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.dfm}


procedure TfrmPrincipal.btncarregarClick(Sender: TObject);
begin
  ACBrNFSe1.NotasFiscais.Clear;
  if OpenDialog1.Execute then
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmPrincipal.btnImprimirClick(Sender: TObject);
begin
  Configuracao;
  if lstbxFR3.ItemIndex = -1 then
    raise Exception.Create('Selecione um arquivo fr3 ');

  if ACBrNFSe1.NotasFiscais.Count = 0 then
    raise Exception.Create('Não foi carregado nenhum xml para impressão');

  ACBrNFSeDANFSeFR1.FastFile := lstbxFR3.Items[lstbxFR3.ItemIndex];
  ACBrNFSe1.NotasFiscais.Imprimir;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  fsFiles: string;
begin
  for fsFiles in TDirectory.GetFiles('..\Delphi\Report\') do
    if Pos('.fr3', LowerCase(fsFiles)) > 0 then
      lstbxFR3.AddItem(fsFiles, nil);
  ACBrNFSe1.Configuracoes.Geral.PathIniCidades := '..\ArqINI';
end;

procedure TfrmPrincipal.Configuracao;
begin
//  With ACBrNFSeDANFSeFR1 do
//  begin
//    PosCanhoto := TPosRecibo(RbCanhoto.ItemIndex);
//  end;
end;

end.
