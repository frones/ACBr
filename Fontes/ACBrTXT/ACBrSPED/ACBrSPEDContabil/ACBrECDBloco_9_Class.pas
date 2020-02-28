{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrECDBloco_9_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_9, ACBrECDBloco_0_Class;

type
  /// TBloco_9 -
  TBloco_9 = class(TACBrSPED)
  private
    FRegistro9001: TRegistro9001;      /// BLOCO 9 - Registro9001
    FRegistro9900: TRegistro9900List;  /// BLOCO 9 - Lista de Registro9900
    FRegistro9990: TRegistro9990;      /// BLOCO 9 - FRegistro9990
    FRegistro9999: TRegistro9999;      /// BLOCO 9 - Registro9999
	FBloco_0: TBloco_0; 
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    property Registro9001: TRegistro9001     read FRegistro9001 write FRegistro9001;
    property Registro9900: TRegistro9900List read FRegistro9900 write FRegistro9900;
    property Registro9990: TRegistro9990     read FRegistro9990 write FRegistro9990;
    property Registro9999: TRegistro9999     read FRegistro9999 write FRegistro9999;
  end;

implementation

constructor TBloco_9.Create;
begin
  inherited Create;
  FRegistro9001 := TRegistro9001.Create;
  FRegistro9900 := TRegistro9900List.Create;
  FRegistro9990 := TRegistro9990.Create;
  FRegistro9999 := TRegistro9999.Create;

  FRegistro9990.QTD_LIN_9 := 0;
end;

destructor TBloco_9.Destroy;
begin
  FRegistro9001.Free;
  FRegistro9900.Free;
  FRegistro9990.Free;
  FRegistro9999.Free;
  inherited;
end;

procedure TBloco_9.LimpaRegistros;
begin
  FRegistro9900.Clear;

  FRegistro9990.QTD_LIN_9 := 0;
end;

procedure TBloco_9.WriteRegistro9001;
begin
  if Assigned(FRegistro9001) then
  begin
     with FRegistro9001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(9-9001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add( LFill('9001') +
            LFill(IND_DAD, 1) 
            );
       ///
       FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + 1;
     end;
  end;
end;

procedure TBloco_9.WriteRegistro9900;
var
intFor: integer;
begin
  if Assigned(FRegistro9900) then
  begin
     for intFor := 0 to FRegistro9900.Count - 1 do
     begin
        with FRegistro9900.Items[intFor] do
        begin
           Add( LFill('9900') +
                LFill(REG_BLC) +
                LFill(QTD_REG_BLC, 0) 
                );
        end;
     end;
     FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + FRegistro9900.Count + 2;
  end;
end;

procedure TBloco_9.WriteRegistro9990;
begin
  if Assigned(FRegistro9990) then
  begin
     with FRegistro9990 do
     begin
        Add( LFill('9990') +
             LFill(QTD_LIN_9, 0) 
             );
     end;
  end;
end;

procedure TBloco_9.WriteRegistro9999;
begin
  if Assigned(Registro9999) then
  begin
     with FRegistro9999 do
     begin
        Add( LFill('9999') +
             LFill(QTD_LIN, 0)
             );
     end;
  end;
end;

end.
