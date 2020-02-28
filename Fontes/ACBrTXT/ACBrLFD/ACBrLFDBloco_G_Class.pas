{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio								   }	  
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

unit ACBrLFDBloco_G_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_G,
     ACBrTXTClass;

type

  /// BLOCO G: INFORMAÇÕES ECONÔMICO-FISCAIS

  { TBloco_G }

  TBloco_G = class(TACBrLFD3505)
  private
    FRegistroG001: TRegistroG001;
    FRegistroG990: TRegistroG990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroG001;
    procedure WriteRegistroG990;

    property RegistroG001: TRegistroG001 read FRegistroG001 write FRegistroG001;
    property RegistroG990: TRegistroG990 read FRegistroG990 write FRegistroG990;
  end;

implementation

uses StrUtils;

{ TBloco_G }

constructor TBloco_G.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_G.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_G.CriaRegistros;
begin
  FRegistroG001 := TRegistroG001.Create;
  FRegistroG990 := TRegistroG990.Create;

  FRegistroG990.QTD_LIN_G := 0;
end;

procedure TBloco_G.LiberaRegistros;
begin
  FRegistroG001.Free;
  FRegistroG990.Free;
end;

procedure TBloco_G.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_G.WriteRegistroG001;
begin
  if Assigned(FRegistroG001) then
    with FRegistroG001 do
    begin
      Add( LFill('G001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroG990.QTD_LIN_G := FRegistroG990.QTD_LIN_G + 1;
    end;
end;

procedure TBloco_G.WriteRegistroG990;
begin
  if Assigned(FRegistroG990) then
    with FRegistroG990 do
    begin
      QTD_LIN_G := QTD_LIN_G + 1;

      Add( LFill('G990') +
           LFill(QTD_LIN_G, 0) );
    end;
end;

end.
