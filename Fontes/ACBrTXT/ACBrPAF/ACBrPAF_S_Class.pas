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

unit ACBrPAF_S_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_S;

type

  { TPAF_S }

  TPAF_S = class(TACBrTXTClass)
  private
    FRegistroS2: TRegistroS2List;

    procedure WriteRegistroS3(RegS2: TRegistroS2);
    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override; 
    procedure LimpaRegistros;

    procedure WriteRegistroS2;

    property RegistroS2: TRegistroS2List read FRegistroS2 write FRegistroS2;
  end;  

implementation

uses ACBrTXTUtils;

{ TPAF_S }
constructor TPAF_S.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_S.CriaRegistros;
begin
  FRegistroS2 := TRegistroS2List.Create;
end;

destructor TPAF_S.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_S.LiberaRegistros;
begin
  FRegistroS2.Free;
end;

procedure TPAF_S.LimpaRegistros;
begin
  //Limpa os Registros
  LiberaRegistros;
  //Recriar os Registros Limpos
  CriaRegistros;
end;

function OrdenarS2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    FormatDateTime('yyyymmddhhmmss', TRegistroS2(ARegistro1).DT_ABER);

  Reg2 :=
    FormatDateTime('yyyymmddhhmmss', TRegistroS2(ARegistro2).DT_ABER);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_S.WriteRegistroS2;
var
  intFor: integer;
begin
  if Assigned(FRegistroS2) then
  begin
    RegistroS2.Sort(@Ordenars2);

    for intFor := 0 to FRegistroS2.Count - 1 do
    begin
      with FRegistroS2.Items[intFor] do
      begin
        Add( LFill('S2') +
             LFill(CNPJ, 14) +
             LFill(DT_ABER, 'yyyymmddhhmmss') +
             //RFill(SITU, 1) +
             RFill(NUM_MESA, 13, ifThen(RegistroValido, ' ', '?')) +
             LFill(VL_TOT, 13, 2) +
             RFill(COO_CM, 9) +
             RFill(NUM_FAB_CM, 20) );
      end;
    end;

    for intFor := 0 to FRegistroS2.Count - 1 do
      WriteRegistroS3( FRegistroS2.Items[intFor] );

  end;
end;

procedure TPAF_S.WriteRegistroS3(RegS2: TRegistroS2);
var
  intFor: integer;
begin
  if Assigned(RegS2.RegistroS3) then
  begin
    for intFor := 0 to RegS2.RegistroS3.Count - 1 do
    begin
      with RegS2.RegistroS3.Items[intFor] do
      begin
        Add( LFill('S3') +
             LFill(RegS2.CNPJ, 14) +
             LFill(RegS2.DT_ABER, 'yyyymmddhhmmss') +
             RFill(NUM_MESA, 13, ifThen(RegistroValido, ' ', '?')) +
             RFill(COD_ITEM, 14) +
             RFill(DESC_ITEM, 100) +
             LFill(QTDE_ITEM, 7, QTDE_DECIMAL) +
             RFill(UNI_ITEM, 3) +
             LFill(VL_UNIT, 8, VL_DECIMAL) +
             LFill(QTDE_DECIMAL, 1) +
             LFill(VL_DECIMAL, 1) );
      end;
    end;  
  end;

end;

end.
 
