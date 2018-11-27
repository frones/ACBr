{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016   Juliomar Marchetti                   }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 05/09/2016: Juliomar Marchetti
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016   Juliomar Marchetti                   }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 05/09/2016: Juliomar Marchetti
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrPAF_J_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_J;


type
  /// TACBrPAF_J -

  { TPAF_J }

  TPAF_J = class(TACBrTXTClass)
  private
    FRegistroJ1: TRegistroJ1List;   /// Lista de FRegistroJ1

    procedure WriteRegistroJ2(RegJ1: TRegistroJ1);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroJ1;

    property RegistroJ1: TRegistroJ1List read FRegistroJ1 write FRegistroJ1;
  end;

implementation

uses ACBrTXTUtils;

{ TPAF_J }

constructor TPAF_J.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_J.CriaRegistros;
begin
  FRegistroJ1 := TRegistroJ1List.Create;
end;

destructor TPAF_J.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_J.LiberaRegistros;
begin
  FRegistroJ1.Free;
end;

procedure TPAF_J.LimpaRegistros;
begin
  LiberaRegistros;
  CriaRegistros;
end;

function OrdenarJ1(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Nota1, Nota2: LongInt;
begin
  Nota1 := StrToIntDef(TRegistroJ1(ARegistro1).NUMERO_NOTA, 0);
  Nota2 := StrToIntDef(TRegistroJ1(ARegistro2).NUMERO_NOTA, 0);

  if Nota1 < Nota2 then
    Result := -1
  else
  if Nota1 > Nota2 then
    Result := 1
  else
    Result := 0;
end;

procedure TPAF_J.WriteRegistroJ1;
var
  i: Integer;
begin
  if Assigned(FRegistroJ1) then
  begin
    for i := 0 to FRegistroJ1.Count - 1 do
    begin
      with FRegistroJ1.Items[i] do
      begin
        Check(funChecaCNPJ(CNPJ), '(J1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Add(LFill('J1') +
            LFill(CNPJ,14) +
            LFill(DATA_EMISSAO, 'yyyymmdd') +
            LFill(SUBTOTAL,14,2) +
            LFill(DESC_SUBTOTAL,13,2) +
            RFill(INDICADOR_DESC, 1) +
            LFill(ACRES_SUBTOTAL,13,2) +
            RFill(INDICADOR_ACRES,1) +
            LFill(VALOR_LIQUIDO,14,2) +
            RFill(INDICADOR_CANC,1) +
            LFill(VAL_CANC_ACRES,13) +
            RFill(ORDEM_APLIC_DES_ACRES,1) +
            RFill(NOME_CLIENTE,40) +
            LFill(CPFCNPJ_CLIENTE,14) +
            LFill(NUMERO_NOTA,10) +
            RFill(SERIE_NOTA,3) +
            LFill(CHAVE_NF,44) +
            LFill(TIPO_DOC[1], 2, False, IfThen(RegistroValido, '0', '?'))
            );
      end;
    end;

    //Registro Filho
    for i := 0 to FRegistroJ1.Count - 1 do
      WriteRegistroJ2( FRegistroJ1.Items[i] );
  end;
end;

function OrdenarJ2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Item1, Item2: LongInt;
begin
  Item1 := StrToInt(TRegistroJ2(ARegistro1).NUMERO_ITEM);
  Item2 := StrToInt(TRegistroJ2(ARegistro2).NUMERO_ITEM);

  if Item1 < Item2 then
    Result := -1
  else
  if Item1 > Item2 then
    Result := 1
  else
    Result := 0;
end;

procedure TPAF_J.WriteRegistroJ2(RegJ1: TRegistroJ1);
var
  i: Integer;
begin
  if Assigned(RegJ1.RegistroJ2) then
  begin
    for i := 0 to RegJ1.RegistroJ2.Count - 1 do
    begin
      with RegJ1.RegistroJ2.Items[i] do
      begin
        Add(LFill('J2') +
            LFill(RegJ1.CNPJ, 14) +
            LFill(DATA_EMISSAO,'yyyymmdd') +
            LFill(NUMERO_ITEM,3) +
            RFill(CODIGO_PRODUTO,14) +
            RFill(DESCRICAO,100) +
            LFill(QUANTIDADE,7) +
            RFill(UNIDADE,3) +
            LFill(VALOR_UNITARIO,8) +
            LFill(DESCONTO_ITEM,8,2) +
            LFill(ACRESCIMO_ITEM,8,2) +
            LFill(VALOR_LIQUIDO,14,2) +
            RFill(TOTALIZADOR_PARCIAL,7) +
            RFill(CASAS_DECIMAIS_QTDE,1) +
            RFill(CASAS_DECIMAIS_VAL_UNIT,1) +
            LFill(NUMERO_NOTA,10) +
            RFill(SERIE_NOTA,3) +
            LFill(CHAVE_NF,44) +
            LFill(TIPO_DOC[1], 2, False, IfThen(RegistroValido, '0', '?'))
            );
      end;

    end;
  end;
end;

end.
