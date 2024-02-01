{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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

unit frMenuTEF; 

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

{$R *.dfm}

  { TFormMenuTEF }

  TFormMenuTEF = class(TForm)
     btOK : TBitBtn;
     btCancel : TBitBtn;
     btVoltar: TBitBtn;
     lbOpcoes : TListBox;
     mOpcao : TMemo ;
     pTitulo : TPanel;
     Panel2 : TPanel;
     Splitter1 : TSplitter ;
     procedure FormShow(Sender: TObject);
     procedure lbOpcoesClick(Sender: TObject);
     procedure lbOpcoesKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    FUsaTeclasDeAtalho: Boolean;
    FNumeroPressionado: Boolean;
    function GetItemSelecionado: Integer;
    function GetOpcoes: TStrings;
    function GetTitulo: String;
    procedure SetItemSelecionado(AValue: Integer);
    procedure SetOpcoes(AValue: TStrings);
    procedure SetTitulo(AValue: String);
  public
    { public declarations }
    property Titulo: String read GetTitulo write SetTitulo;
    property Opcoes: TStrings read GetOpcoes write SetOpcoes;
    property ItemSelecionado: Integer read GetItemSelecionado write SetItemSelecionado;
    property UsaTeclasDeAtalho: Boolean read FUsaTeclasDeAtalho write FUsaTeclasDeAtalho;
  end; 

implementation

uses
  ACBrUtil.Strings;

{ TFormMenuTEF }

procedure TFormMenuTEF.FormShow(Sender: TObject);
begin
   if mOpcao.Lines.Count > 0 then
   begin
     mOpcao.Width   := Trunc(Width/2)-10;
     mOpcao.Visible := True ;
     Splitter1.Visible := True ;
   end ;

   lbOpcoes.SetFocus;
   if lbOpcoes.Items.Count > 0 then
      lbOpcoes.ItemIndex := 0 ;

   FNumeroPressionado := False;
end;

procedure TFormMenuTEF.lbOpcoesClick(Sender: TObject);
begin
  if FUsaTeclasDeAtalho and FNumeroPressionado then
    ModalResult := mrOK;
end;

procedure TFormMenuTEF.lbOpcoesKeyPress(Sender: TObject; var Key: char);
begin
  FNumeroPressionado := CharIsNum(Key);
end;

function TFormMenuTEF.GetTitulo: String;
begin
  Result := pTitulo.Caption;
end;

procedure TFormMenuTEF.SetItemSelecionado(AValue: Integer);
begin
  lbOpcoes.ItemIndex := AValue;
end;

function TFormMenuTEF.GetOpcoes: TStrings;
begin
  Result := lbOpcoes.Items;
end;

function TFormMenuTEF.GetItemSelecionado: Integer;
begin
  Result := lbOpcoes.ItemIndex;
end;

procedure TFormMenuTEF.SetOpcoes(AValue: TStrings);
begin
  lbOpcoes.Items.Assign(AValue);
end;

procedure TFormMenuTEF.SetTitulo(AValue: String);
begin
  pTitulo.Caption := AValue;
end;

end.

