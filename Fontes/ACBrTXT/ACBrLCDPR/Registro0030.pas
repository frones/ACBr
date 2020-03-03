{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Hübner e Elton Barbosa (EMBarbosa)      }
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
unit Registro0030;

interface

type
  TRegistro0030 = Class
  private
    FEMAIL: String;
    FBAIRRO: String;
    FNUM: String;
    FUF: String;
    FNUM_TEL: string;
    FCEP: String;
    FCOMPL: String;
    FCOD_MUN: String;
    FENDERECO: String;
    procedure SetBAIRRO(const Value: String);
    procedure SetCEP(const Value: String);
    procedure SetCOD_MUN(const Value: String);
    procedure SetCOMPL(const Value: String);
    procedure SetEMAIL(const Value: String);
    procedure SetENDERECO(const Value: String);
    procedure SetNUM(const Value: String);
    procedure SetNUM_TEL(const Value: string);
    procedure SetUF(const Value: String);
  public
    property ENDERECO : String read FENDERECO write SetENDERECO;
    property NUM : String read FNUM write SetNUM;
    property COMPL : String read FCOMPL write SetCOMPL;
    property BAIRRO : String read FBAIRRO write SetBAIRRO;
    property UF : String read FUF write SetUF;
    property COD_MUN : String read FCOD_MUN write SetCOD_MUN;
    property CEP : String read FCEP write SetCEP;
    property NUM_TEL : string read FNUM_TEL write SetNUM_TEL;
    property EMAIL : String read FEMAIL write SetEMAIL;
  End;

implementation

uses
  SysUtils;

{ TRegistro0030 }

procedure TRegistro0030.SetBAIRRO(const Value: String);
begin
  if Length(Value) > 50 then
    raise Exception.Create('BAIRRO - Tamanho máximo permitido é 50 caracteres!');

  FBAIRRO := Value;
end;

procedure TRegistro0030.SetCEP(const Value: String);
begin
  if Length(Value) > 8 then
    raise Exception.Create('CEP - Tamanho máximo permitido é 8 caracteres!');

  FCEP := Value;
end;

procedure TRegistro0030.SetCOD_MUN(const Value: String);
begin
  if Length(Value) > 7 then
    raise Exception.Create('COD_MUN - Tamanho máximo permitido é 7 caracteres!');

  FCOD_MUN := Value;
end;

procedure TRegistro0030.SetCOMPL(const Value: String);
begin
  if Length(Value) > 50 then
    raise Exception.Create('COMPL - Tamanho máximo permitido é 50 caracteres!');

  FCOMPL := Value;
end;

procedure TRegistro0030.SetEMAIL(const Value: String);
begin
  if Length(Value) > 115 then
    raise Exception.Create('EMAIL - Tamanho máximo permitido é 115 caracteres!');

  FEMAIL := Value;
end;

procedure TRegistro0030.SetENDERECO(const Value: String);
begin
  if Length(Value) > 150 then
    raise Exception.Create('ENDERECO - Tamanho máximo permitido é 150 caracteres!');

  FENDERECO := Value;
end;

procedure TRegistro0030.SetNUM(const Value: String);
begin
  if Length(Value) > 6 then
    raise Exception.Create('NUM - Tamanho máximo permitido é 6 caracteres!');

  FNUM := Value;
end;

procedure TRegistro0030.SetNUM_TEL(const Value: string);
begin
  if Length(Value) > 15 then
    raise Exception.Create('NUM_TEL - Tamanho máximo permitido é 15 caracteres!');

  FNUM_TEL := Value;
end;

procedure TRegistro0030.SetUF(const Value: String);
begin
  if Length(Value) > 2 then
    raise Exception.Create('UF - Tamanho máximo permitido é 2 caracteres!');

  FUF := Value;
end;

end.
