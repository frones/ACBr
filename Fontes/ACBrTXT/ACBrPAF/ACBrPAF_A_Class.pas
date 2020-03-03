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

{$I ACBr.inc}

unit ACBrPAF_A_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass, ACBrTXTUtils,
     ACBrPAF_A;

type
  TPAF_A = class(TACBrTXTClass)
  private
    FRegistroA2: TRegistroA2List;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    procedure WriteRegistroA2;

    property RegistroA2: TRegistroA2List read FRegistroA2 write FRegistroA2;
  end;

implementation

{ TPAF_A }

constructor TPAF_A.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_A.CriaRegistros;
begin
  FRegistroA2 := TRegistroA2List.Create;
end;

destructor TPAF_A.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_A.LiberaRegistros;
begin
  FRegistroA2.Free;
end;

procedure TPAF_A.LimpaRegistros;
begin
  //Limpa os Registros
  LiberaRegistros;
  //Recriar os Registros Limpos
  CriaRegistros;
end;

function OrdenarA2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    FormatDateTime('YYYYMMDD', TRegistroA2(ARegistro1).DT) +
    Format('%-25s', [TRegistroA2(ARegistro1).MEIO_PGTO]) +
    Format('%-1s', [TRegistroA2(ARegistro1).TIPO_DOC]);

  Reg2 :=
    FormatDateTime('YYYYMMDD', TRegistroA2(ARegistro2).DT) +
    Format('%-25s', [TRegistroA2(ARegistro2).MEIO_PGTO]) +
    Format('%-1s', [TRegistroA2(ARegistro2).TIPO_DOC]);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_A.WriteRegistroA2;
var
  intFor: integer;
begin
  if Assigned(FRegistroA2) then
  begin
    FRegistroA2.Sort(@OrdenarA2);

    for intFor := 0 to FRegistroA2.Count - 1 do
    begin
      with FRegistroA2.Items[intFor] do
      begin
        Add(LFill('A2') +
            LFill(DT, 'yyyymmdd') +
            RFill(MEIO_PGTO, 25, IfThen(RegistroValido, ' ', '?')) +
            RFill(TIPO_DOC, 1) +
            LFill(VL, 12, 2));
      end;
    end;
  end;
end;

end.
 
