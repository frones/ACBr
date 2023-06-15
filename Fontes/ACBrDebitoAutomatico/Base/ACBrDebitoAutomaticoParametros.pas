{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDebitoAutomaticoParametros;

interface

uses
  Classes, SysUtils,
  ACBrDebitoAutomaticoConversao;

type

  { TConta }
{
  TConta = class
  private
    FAgenciaCodigo: Integer;
    FAgenciaDV: string;
    FContaNumero: Int64;
    FContaDV: string;
    FDV: string;
    FTipoConta: Integer;
  public
    property AgenciaCodigo: Integer read FAgenciaCodigo write FAgenciaCodigo;
    property AgenciaDV: string read FAgenciaDV write FAgenciaDV;
    property ContaNumero: Int64 read FContaNumero write FContaNumero;
    property ContaDV: string read FContaDV write FContaDV;
    property DV: string read FDV write FDV;
    property TipoConta: Integer read FTipoConta write FTipoConta;
  end;
}
  { TEndereco }
{
  TEndereco = class
  private
    FLogradouro: string;
    FNumero: string;
    FComplemento: string;
    FCidade: string;
    FCEP: Integer;
    FEstado: string;
  public
    property Logradouro: string read FLogradouro write FLogradouro;
    property Numero: string read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property Cidade: string read FCidade write FCidade;
    property CEP: Integer read FCEP write FCEP;
    property Estado: string read FEstado write FEstado;
  end;
}
  { TEmpresa }
{
  TEmpresa = class
  private
//    FTipoInscricao: TTipoInscricao;
    FNumeroInscricao: string;
    FConvenio: string;
    FNome: string;
    FConta: TConta;
    FEndereco: TEndereco;
  public
    Constructor Create;
    destructor Destroy; override;

//    property TipoInscricao: TTipoInscricao read FTipoInscricao write FTipoInscricao;
    property NumeroInscricao: string read FNumeroInscricao write FNumeroInscricao;
    property Convenio: string read FConvenio write FConvenio;
    property Nome: string read FNome write FNome;

    property Conta: TConta read FConta write FConta;
    property Endereco: TEndereco read FEndereco write FEndereco;
  end;
}
  { TConfigGeral }

  TConfigGeral = class
  private
//    FEmpresa: TEmpresa;
//    FUsarDadosConfig: Boolean;
    FCNPJEmpresa: string;
  public
    constructor Create;
    destructor Destroy; override;

//    property Empresa: TEmpresa read FEmpresa write FEmpresa;
//    property UsarDadosConfig: Boolean read FUsarDadosConfig write FUsarDadosConfig;
    property CNPJEmpresa: string read FCNPJEmpresa write FCNPJEmpresa;
  end;

implementation

{ TConfigGeral }

constructor TConfigGeral.Create;
begin
  inherited Create;

//  FEmpresa := TEmpresa.Create;
end;

destructor TConfigGeral.Destroy;
begin
//  FEmpresa.Free;

  inherited Destroy;
end;

{ TEmpresa }
{
constructor TEmpresa.Create;
begin
  inherited Create;

  FConta := TConta.Create;
  FEndereco := TEndereco.Create;
end;

destructor TEmpresa.Destroy;
begin
  FConta.Free;
  FEndereco.Free;

  inherited Destroy;
end;
}
end.
