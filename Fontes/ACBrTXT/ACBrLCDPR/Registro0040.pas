{******************************************************************************}
{ Projeto: Componente ACBrLCDPR                                                }
{  Biblioteca multiplataforma de componentes Delphi para geração do LCDPR -    }
{ Lirvro Caixa Digital do Produtor Rural                                       }
{                                                                              }
{                                                                              }
{ Desenvolvimento e doação ao Projeto ACBr: Willian Hübner                     }
{                                                                              }
{ Ajustes e correções para doação: Elton Barbosa (EMBarbosa)                   }
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
unit Registro0040;

interface

uses LCDPRBlocos;

type
  TRegistro0040 = Class
  private
    FBAIRRO: String;
    FNUM: String;
    FCOD_IMOVEL: Integer;
    FTIPO_EXPLORACAO: TTipoExploracao;
    FUF: String;
    FNOME_IMOVEL: String;
    FCAD_ITR: Integer;
    FCEP: String;
    FMOEDA: String;
    FCOMPL: String;
    FCOD_MUN: String;
    FINSCR_ESTADUAL: String;
    FPARTICIPACAO: Double;
    FENDERECO: String;
    FPAIS: String;
    FCAEPF: string;
    procedure SetBAIRRO(const Value: String);
    procedure SetCAD_ITR(const Value: Integer);
    procedure SetCEP(const Value: String);
    procedure SetCOD_IMOVEL(const Value: Integer);
    procedure SetCOD_MUN(const Value: String);
    procedure SetCOMPL(const Value: String);
    procedure SetENDERECO(const Value: String);
    procedure SetINSCR_ESTADUAL(const Value: String);
    procedure SetMOEDA(const Value: String);
    procedure SetNOME_IMOVEL(const Value: String);
    procedure SetNUM(const Value: String);
    procedure SetPAIS(const Value: String);
    procedure SetPARTICIPACAO(const Value: Double);
    procedure SetTIPO_EXPLORACAO(const Value: TTipoExploracao);
    procedure SetUF(const Value: String);
    procedure SetCAEPF(const Value: string);
  public
    property COD_IMOVEL : Integer read FCOD_IMOVEL write SetCOD_IMOVEL;
    property PAIS : String read FPAIS write SetPAIS;
    property MOEDA : String read FMOEDA write SetMOEDA;
    property CAD_ITR : Integer read FCAD_ITR write SetCAD_ITR;
    property CAEPF : string read FCAEPF write SetCAEPF;
    property INSCR_ESTADUAL : String read FINSCR_ESTADUAL write SetINSCR_ESTADUAL;
    property NOME_IMOVEL : String read FNOME_IMOVEL write SetNOME_IMOVEL;
    property ENDERECO : String read FENDERECO write SetENDERECO;
    property NUM : String read FNUM write SetNUM;
    property COMPL : String read FCOMPL write SetCOMPL;
    property BAIRRO : String read FBAIRRO write SetBAIRRO;
    property UF : String read FUF write SetUF;
    property COD_MUN : String read FCOD_MUN write SetCOD_MUN;
    property CEP : String read FCEP write SetCEP;
    property TIPO_EXPLORACAO : TTipoExploracao read FTIPO_EXPLORACAO write SetTIPO_EXPLORACAO;
    property PARTICIPACAO : Double read FPARTICIPACAO write SetPARTICIPACAO;
  End;

implementation

uses
  SysUtils, ACBrUtil;

{ TRegistro0040 }

procedure TRegistro0040.SetBAIRRO(const Value: String);
begin
  if Length(Value) > 50 then
    raise Exception.Create('BAIRRO - Tamanho máximo permitido é 50 caracteres!');

  FBAIRRO := Value;
end;

procedure TRegistro0040.SetCAD_ITR(const Value: Integer);
begin
  if Length(IntToStr(Value)) > 8 then
    raise Exception.Create('CAD_ITR - Tamanho máximo permitido é 8 caracteres!');

  FCAD_ITR := Value;
end;

procedure TRegistro0040.SetCAEPF(const Value: string);
begin
  if Length(Value) > 14
 then
    raise Exception.Create('CAEPF - Tamanho máximo permitido é 14 caracteres!');

  FCAEPF := Value;
end;

procedure TRegistro0040.SetCEP(const Value: String);
var
  TempCep: string;
begin
  TempCep := OnlyNumber(Value);
  if Length(TempCep) > 8 then
    raise Exception.Create('CEP - Tamanho máximo permitido é 8 caracteres!');

  FCEP := TempCep;
end;

procedure TRegistro0040.SetCOD_IMOVEL(const Value: Integer);
begin
  FCOD_IMOVEL := Value;
end;

procedure TRegistro0040.SetCOD_MUN(const Value: String);
begin
  if Length(Value) > 7 then
    raise Exception.Create('COD_MUN - Tamanho máximo permitido é 7 caracteres!');

  FCOD_MUN := Value;
end;

procedure TRegistro0040.SetCOMPL(const Value: String);
begin
  if Length(Value) > 50 then
    raise Exception.Create('COMPL - Tamanho máximo permitido é 50 caracteres!');

  FCOMPL := Value;
end;

procedure TRegistro0040.SetENDERECO(const Value: String);
begin
  if Length(Value) > 150 then
    raise Exception.Create('ENDERECO - Tamanho máximo permitido é 150 caracteres!');

  FENDERECO := Value;
end;

procedure TRegistro0040.SetINSCR_ESTADUAL(const Value: String);
var
  TempIE: string;
begin
  TempIE := OnlyNumber(Value);
  if Length(TempIE) > 14 then
    raise Exception.Create('INSCR_ESTADUAL - Tamanho máximo permitido é 14 caracteres!');

  FINSCR_ESTADUAL := TempIE;
end;

procedure TRegistro0040.SetMOEDA(const Value: String);
begin
  if Length(Value) > 3 then
    raise Exception.Create('MOEDA - Tamanho máximo permitido é 3 caracteres!');

  FMOEDA := Value;
end;

procedure TRegistro0040.SetNOME_IMOVEL(const Value: String);
begin
  if Length(Value) > 50 then
    raise Exception.Create('NOME_IMOVEL - Tamanho máximo permitido é 50 caracteres!');

  FNOME_IMOVEL := Value;
end;

procedure TRegistro0040.SetNUM(const Value: String);
begin
  if Length(Value) > 6 then
    raise Exception.Create('NUM - Tamanho máximo permitido é 6 caracteres!');

  FNUM := Value;
end;

procedure TRegistro0040.SetPAIS(const Value: String);
begin
  if Length(Value) > 3 then
    raise Exception.Create('PAIS - Tamanho máximo permitido é 3 caracteres!');

  FPAIS := Value;
end;

procedure TRegistro0040.SetPARTICIPACAO(const Value: Double);
begin
  FPARTICIPACAO := Value;
end;

procedure TRegistro0040.SetTIPO_EXPLORACAO(const Value: TTipoExploracao);
begin
  FTIPO_EXPLORACAO := Value;
end;

procedure TRegistro0040.SetUF(const Value: String);
begin
  if Length(Value) > 2 then
    raise Exception.Create('UF - Tamanho máximo permitido é 2 caracteres!');

  FUF := Value;
end;

end.
