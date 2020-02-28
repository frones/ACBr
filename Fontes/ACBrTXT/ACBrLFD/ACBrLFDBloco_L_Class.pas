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

unit ACBrLFDBloco_L_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_L,
     ACBrTXTClass;

type

  /// BLOCO L: REGISTROS DE NATUREZA FINANCEIRA E ORÇAMENTÁRIA

  { TBloco_L }

  TBloco_L = class(TACBrLFD3505)
  private
    FRegistroL001: TRegistroL001;
    FRegistroL990: TRegistroL990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroL001;
    procedure WriteRegistroL990;

    property RegistroL001: TRegistroL001 read FRegistroL001 write FRegistroL001;
    property RegistroL990: TRegistroL990 read FRegistroL990 write FRegistroL990;
  end;

implementation

uses StrUtils;

{ TBloco_L }

constructor TBloco_L.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_L.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_L.CriaRegistros;
begin
  FRegistroL001 := TRegistroL001.Create;
  FRegistroL990 := TRegistroL990.Create;

  FRegistroL990.QTD_LIN_L:= 0;
end;

procedure TBloco_L.LiberaRegistros;
begin
  FRegistroL001.Free;
  FRegistroL990.Free;
end;

procedure TBloco_L.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_L.WriteRegistroL001;
begin
  if Assigned(FRegistroL001) then
    with FRegistroL001 do
    begin
      Add( LFill('L001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroL990.QTD_LIN_L := FRegistroL990.QTD_LIN_L + 1;
    end;
end;

procedure TBloco_L.WriteRegistroL990;
begin
  if Assigned(FRegistroL990) then
    with FRegistroL990 do
    begin
      QTD_LIN_L := QTD_LIN_L + 1;

      Add( LFill('L990') +
           LFill(QTD_LIN_L, 0) );
    end;
end;

end.
