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

unit uListaMensagem;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ACBrSMSClass;

type
  TfrmListaMensagem = class(TForm)
    GroupBox4: TGroupBox;
    btnListarMensagens: TButton;
    rgdFiltroMsg: TRadioGroup;
    lstMensagens: TListView;
    rdgBandeja: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure btnListarMensagensClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmListaMensagem: TfrmListaMensagem;

implementation

uses
  uPrincipal{, ACBrSMSClass};

{$R *.lfm}

procedure TfrmListaMensagem.FormCreate(Sender: TObject);
begin
  lstMensagens.Clear;

  rdgBandeja.Enabled := frmPrincipal.ACBrSMS1.BandejasSimCard > 1;
  if rdgBandeja.Enabled then
    rdgBandeja.ItemIndex := Integer(frmPrincipal.ACBrSMS1.SimCard);
end;

procedure TfrmListaMensagem.btnListarMensagensClick(Sender: TObject);
var
  Filtro: TACBrSMSFiltro;
begin
  lstMensagens.Clear;

  if frmPrincipal.ACBrSMS1.BandejasSimCard > 1 then
  begin
    if rdgBandeja.ItemIndex = 0 then
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard1)
    else
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard2);
  end;

  case rgdFiltroMsg.ItemIndex of
    1: Filtro := fltLidas;
    2: Filtro := fltNaoLidas;
  else
    Filtro := fltTudo;
  end;

  frmPrincipal.ACBrSMS1.ListarMensagens(Filtro, 'mensagens.txt');
  ShowMessage('Pronto.');
end;
end.
