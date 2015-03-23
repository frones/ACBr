{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrFContBloco_0;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrFContBlocos;

type
  /// Registro 0000 - ABERTURA  DO  ARQUIVO  DIGITAL  E  IDENTIFICAÇÃO  DO
  ///                 EMPRESÁRIO OU DA SOCIEDADE EMPRESÁRIA

  TRegistro0000 = class
  private
    fDT_INI: TDateTime;       /// Data inicial das informações contidas no arquivo
    fDT_FIN: TDateTime;       /// Data final das informações contidas no arquivo
    fNOME: String;        /// Nome empresarial do empresário ou sociedade empresária.
    fCNPJ: String;        /// Número de inscrição do empresário ou sociedade empresária no CNPJ.
    fUF: String;          /// Sigla da unidade da federação do empresário ou sociedade empresária.
    fIE: String;          /// Inscrição Estadual do empresário ou sociedade empresária.
    fCOD_MUN: String;     /// Código do município do domicílio fiscal do empresário ou sociedade empresária, conforme tabela do IBGE - Instituto Brasileiro de Geografia e Estatística.
    fIM: String;          /// Inscrição Municipal do empresário ou sociedade empresária.
    fIND_SIT_ESP: String; /// Indicador de situação especial (conforme tabela publicada pelo Sped).
    fIND_SIT_INI_PER: String; //Indicador do início do período conforme Tabela de Indicador do início do período.
  public
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property NOME: String read fNOME write fNOME;
    property CNPJ: String read fCNPJ write fCNPJ;
    property UF: String read fUF write fUF;
    property IE: String read fIE write fIE;
    property COD_MUN: String read fCOD_MUN write fCOD_MUN;
    property IM: String read fIM write fIM;
    property IND_SIT_ESP: String read fIND_SIT_ESP write fIND_SIT_ESP;
    property IND_SIT_INI_PER: String read fIND_SIT_INI_PER write fIND_SIT_INI_PER;
  end;

implementation

{ TRegistro0007List }

end.
