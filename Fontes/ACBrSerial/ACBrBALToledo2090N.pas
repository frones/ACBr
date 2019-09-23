{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrBALToledo2090N;

interface

uses
  Classes, ACBrBALClass;

type

  { TACBrBALToledo2090N }

  TACBrBALToledo2090N = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end ;

implementation

uses
  SysUtils, Math, StrUtils, 
  ACBrConsts, ACBrUtil,
  {$IFDEF COMPILER6_UP}
   DateUtils 
  {$ELSE} 
   ACBrD5, synaser, Windows
  {$ENDIF};

{ TACBrBALToledo2090N }

constructor TACBrBALToledo2090N.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Toledo 2090N' ;
end;

function TACBrBALToledo2090N.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
Var
  wResposta : AnsiString ;
  wDecimais : Integer ;
  Protocolo: String;
  wPosIni, wPosFim: Integer;
  Peso_Tara: String;
begin
  Protocolo        := '';
  Peso_Tara        := '';

  wDecimais := 1000 ;

  { Protocolo Ethernet sem Criptografia
    [ STX ] [ OPCODE ] [ DADOS ] [ DLE] [ ETX ] [ CS ]

    OPCODE = 2 bytes - ASCII "02"

    DADOS : SWA = 1 byte
            SWB = 1 byte
            SWC = 1 byte
            PESO = 6 bytes
            TARA = 6 bytes ____ Até aqui segue um padrão o demais dados abaixo varia conforme o modelo
            PEÇAS = 6 bytes
            PMP = 6 bytes
            CÓDIGO = 11 bytes
            Reservado = 1 byte
            Habilita Escrita = 1 byte
                               0 = Não permite
                               1 = Permite somente pela página Web
                               2 = Permite somente pelo Easylink
                               3 = Permite pela página Web e pelo Easylink
            Capacidade = 1 byte
            Flag da Cap. de Zero = 1 byte
                                   "P" - Acima de zero
                                   "N" - Abaixo de zero
            Captura de zero = 6 bytes
            Consecutivo = 6 bytes
            Classificaçãodo Peso = 1 byte
            Memória Utilizada = 1 byte

    DLE = 1 byte - 10
    ETX = 1 byte - 03
    CS = 1 byte - Checksum. O cálculo do checksum é feito pelo complemento
       de 2 da soma de todos os bytes transmitidos de OPCODE e DADOS.
  }

  wPosIni := Pos(STX, aResposta);
  wPosFim := PosEx(ETX, aResposta, wPosIni + 1);

  if (wPosFim > 0) then
    Protocolo := 'Protocolo Eth'
  else
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  // Contem a String inteira
  fpUltimaResposta := Copy(aResposta, wPosIni, wPosFim - wPosIni);

  // Contem somente o Peso
  wResposta := Trim(Copy(aResposta, wPosIni + 4, 6));

  // Contem somente o Peso da Tara
  Peso_Tara := (Trim(Copy(aResposta, wPosIni + 10, 6)));

  if Length(wResposta) > 0 then
  begin
    { Ajustando o separador de Decimal corretamente }
    wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
    wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

    try
       if pos(DecimalSeparator,wResposta) > 0 then  { Já existe ponto decimal ? }
          Result := StrToFloat(wResposta)
       else
          Result := StrToInt(wResposta) / wDecimais ;

       if (StrToFloat(Peso_Tara) <= 0) then
       begin
         case AnsiIndexText(Copy(aResposta, 3, 1), ['x','r','s']) of
           0 : Result := -1  ;  { Instavel }
           1 : Result := -2  ;  { Peso Negativo }
           2 : Result := -10 { Sobrecarga de Peso }
         end;
       end
       else
       begin
         case AnsiIndexText(Copy(aResposta, 3, 1), ['y','s']) of
           0 : Result := -1  ;  { Instavel }
           1 : Result := 0  ;  { Aguardando Peso }
         end;
       end;
    except
      Result := 0 ;
    end;
  end
  else
    Result := 0;

end;

end.
