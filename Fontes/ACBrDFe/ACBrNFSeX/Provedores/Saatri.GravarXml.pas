{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Saatri.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXConversao, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_Saatri201 }

  TNFSeW_Saatri201 = class(TNFSeW_ABRASFv2)
  protected
    procedure DefinirIDDeclaracao; override;
    procedure DefinirIDRps; override;

    procedure Configuracao; override;

  end;

  { TNFSeW_Saatri203 }

  TNFSeW_Saatri203 = class(TNFSeW_Saatri201)
  protected

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Saatri
//==============================================================================

{ TNFSeW_Saatri201 }

procedure TNFSeW_Saatri201.Configuracao;
begin
  inherited Configuracao;

  NrOcorrValorISS := 1;
  NrOcorrAliquota := 1;

  GerarIDRps := True;
end;

procedure TNFSeW_Saatri201.DefinirIDDeclaracao;
begin
  NFSe.InfID.ID := 'Declaracao_' +
                      OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj);
end;

procedure TNFSeW_Saatri201.DefinirIDRps;
begin
  NFSe.InfID.ID := 'rps' + OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie +
                    FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo);
end;

end.
