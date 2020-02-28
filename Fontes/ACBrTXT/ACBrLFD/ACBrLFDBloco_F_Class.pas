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

unit ACBrLFDBloco_F_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_F,
     ACBrTXTClass;

type

  /// BLOCO F: LIVROS E MAPAS DE REGISTRO DAS OPERAÇÕES DE CONTROLE

  { TBloco_F }

  TBloco_F = class(TACBrLFD3505)
  private
    FRegistroF001: TRegistroF001;
    FRegistroF990: TRegistroF990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroF001;
    procedure WriteRegistroF990;

    property RegistroF001: TRegistroF001 read FRegistroF001 write FRegistroF001;
    property RegistroF990: TRegistroF990 read FRegistroF990 write FRegistroF990;
  end;

implementation

uses StrUtils;

{ TBloco_F }

constructor TBloco_F.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_F.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_F.CriaRegistros;
begin  
  FRegistroF001 := TRegistroF001.Create;
  FRegistroF990 := TRegistroF990.Create;

  FRegistroF990.QTD_LIN_F := 0;
end;

procedure TBloco_F.LiberaRegistros;
begin
  FRegistroF001.Free;
  FRegistroF990.Free;
end;

procedure TBloco_F.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_F.WriteRegistroF001;
begin
  if Assigned(FRegistroF001) then
    with FRegistroF001 do
    begin
      Add( LFill('F001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroF990.QTD_LIN_F := FRegistroF990.QTD_LIN_F + 1;
    end;
end;

procedure TBloco_F.WriteRegistroF990;
begin
  if Assigned(FRegistroF990) then
    with FRegistroF990 do
    begin
      QTD_LIN_F := QTD_LIN_F + 1;

      Add( LFill('F990') +
           LFill(QTD_LIN_F, 0) );
    end;
end;

end.
