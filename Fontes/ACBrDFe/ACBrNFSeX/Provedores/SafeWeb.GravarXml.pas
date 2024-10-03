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

unit SafeWeb.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase,
  ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_SafeWeb200 }

  TNFSeW_SafeWeb200 = class(TNFSeW_ABRASFv2)
  protected
    procedure DefinirIDDeclaracao; override;
    procedure DefinirIDRps; override;

    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SafeWeb
//==============================================================================

{ TNFSeW_SafeWeb200 }

procedure TNFSeW_SafeWeb200.Configuracao;
begin
  inherited Configuracao;

  FormatoEmissao := tcDatHor;
  FormatoCompetencia := tcDatHor;
  FormatoAliq := tcDe2;

  NrOcorrAliquota := 1;

  GerarIDDeclaracao := False;
  GerarIDRps := True;
end;

procedure TNFSeW_SafeWeb200.DefinirIDDeclaracao;
begin
  NFSe.InfID.ID := '';
end;

procedure TNFSeW_SafeWeb200.DefinirIDRps;
begin
  NFSe.InfID.ID := 'rps' + OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie;
end;

end.
