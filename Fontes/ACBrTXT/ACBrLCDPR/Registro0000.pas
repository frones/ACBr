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
unit Registro0000;

interface

uses
  LCDPRBlocos, LCDPRUtils;

type
  TRegistro0000 = Class
  private
    FDT_INI: TDateTime;
    FDT_SIT_ESP: TDateTime;
    FIND_SIT_INI_PER: TIndInicio;
    FCPF: String;
    FCOD_VER: TCodVer;
    FDT_FIN: TDateTime;
    FSIT_ESPECIAL: TIndSitEsp;
    FNOME: String;
    procedure SetCOD_VER(const Value: TCodVer);
    procedure SetCPF(const Value: String);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);
    procedure SetDT_SIT_ESP(const Value: TDateTime);
    procedure SetIND_SIT_INI_PER(const Value: TIndInicio);
    procedure SetNOME(const Value: String);
    procedure SetSIT_ESPECIAL(const Value: TIndSitEsp);
  public
    property COD_VER : TCodVer read FCOD_VER write SetCOD_VER;
    property CPF : String read FCPF write SetCPF;
    property NOME : String read FNOME write SetNOME;
    property IND_SIT_INI_PER : TIndInicio read FIND_SIT_INI_PER write SetIND_SIT_INI_PER;
    property SIT_ESPECIAL : TIndSitEsp read FSIT_ESPECIAL write SetSIT_ESPECIAL;
    property DT_SIT_ESP : TDateTime read FDT_SIT_ESP write SetDT_SIT_ESP;
    property DT_INI : TDateTime read FDT_INI write SetDT_INI;
    property DT_FIN : TDateTime read FDT_FIN write SetDT_FIN;
  End;

implementation

uses
  SysUtils;

{ TRegistro0000 }

procedure TRegistro0000.SetCOD_VER(const Value: TCodVer);
begin
  FCOD_VER := Value;
end;

procedure TRegistro0000.SetCPF(const Value: String);
begin
  if (Length(SoNumeros(Value)) <> 11) then
    raise Exception.Create('CPF precisa possuir 11 caracteres!');

  FCPF := SoNumeros(Value);
end;

procedure TRegistro0000.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;
end;

procedure TRegistro0000.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;
end;

procedure TRegistro0000.SetDT_SIT_ESP(const Value: TDateTime);
begin
  FDT_SIT_ESP := Value;
end;

procedure TRegistro0000.SetIND_SIT_INI_PER(const Value: TIndInicio);
begin
  FIND_SIT_INI_PER := Value;
end;

procedure TRegistro0000.SetNOME(const Value: String);
begin
  FNOME := Value;
end;

procedure TRegistro0000.SetSIT_ESPECIAL(const Value: TIndSitEsp);
begin
  FSIT_ESPECIAL := Value;
end;

end.
