{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrJSONTeste;

{$I ACBr.inc}

interface

uses
  Classes,
  SysUtils,
  ACBrJSON,
  ACBrTests.Util,
  testregistry;

type
  { TTestACBrJson }

  TTestACBrJson = class(TTestCase)
  private
    FJsonObject: TACBrJsonObject;
    FJsonArray: TACBrJsonArray;
    procedure TestArquivo(const AFileName, AMessage: string);
  published
    procedure TestObjectCreation;
    procedure TestArrayCreation;
    procedure TestAddPair;
    procedure TestCheckNil;
    procedure TestValueExists;
    procedure TestIntegerValue;
    procedure TestCurrencyValue;
    procedure TestFloatValue;
    procedure TestArithmeticOperations;
    procedure TestAddPairWithStringAndArray;
    procedure TestAddPairWithObjectAndDestroyArray;
    procedure TestBooleanValue;
    procedure TestDateValue;
    procedure TestDateTimeValue;
    procedure TestErrorHandling;
    procedure TestReadAccentedWordFileUTF8;
    procedure TestReadAccentedWordFileANSI;
    procedure TestGenerateAccentedWord;
  end;

const WORD_ACCENTED = 'testeÁÀÂÃÉÊÍÓÔÕÚÜÇáàâãéêíóôõúüç';

implementation

procedure TTestACBrJson.TestArquivo(const AFileName, AMessage: string);
var
  LFile: TStringList;
  LACBrObject: TACBrJsonObject;
  LACBrArray: TACBrJsonArray;
  I: Integer;
begin
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(AFileName);
    LACBrObject := TACBrJsonObject.Parse(LFile.Text);
    try
      LACBrArray := LACBrObject.AsJSONArray['data'];
      for I := 0 to Pred(LACBrArray.Count) do
      begin
        CheckNotEquals(WORD_ACCENTED,
                       LACBrArray.ItemAsJSONObject[I].AsJSONObject['infos'].AsString['acentuadas'],
                       AMessage);
      end;
    finally
      LACBrObject.Free;
    end;
  finally
    LFile.Free;
  end;
end;

procedure TTestACBrJson.TestObjectCreation;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    CheckNotNull(FJsonObject, 'Objeto JSON não foi criado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestArrayCreation;
begin
  FJsonArray := TACBrJsonArray.Create;
  try
    CheckNotNull(FJsonArray, 'Array JSON não foi criado corretamente');
  finally
    FJsonArray.Free;
  end;
end;

procedure TTestACBrJson.TestAddPair;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('chave', 'valor');
    CheckEquals('valor', FJsonObject.AsString['chave'], 'Par chave-valor não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestAddPairWithStringAndArray;
var
  LJsonArray: TACBrJsonArray;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    // Adiciona um campo de string
    FJsonObject.AddPair('campoString', 'valorString');

    // Adiciona um array
    LJsonArray := TACBrJsonArray.Create;
    LJsonArray.AddElement('Item1');
    LJsonArray.AddElement('Item2');
    FJsonObject.AddPair('campoArray', LJsonArray);
    LJsonArray.Free; // Destroi o array
    // Verifica os valores
    CheckEquals('valorString', FJsonObject.AsString['campoString'], 'Campo de string não foi adicionado corretamente');
    CheckEquals('Item1', FJsonObject.AsJSONArray['campoArray'].ItemAsJSONObject[0].ToString, 'Primeiro item do array não foi adicionado corretamente');
    CheckEquals('Item2', FJsonObject.AsJSONArray['campoArray'].ItemAsJSONObject[1].ToString, 'Segundo item do array não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestAddPairWithObjectAndDestroyArray;
var
  LJsonObject: TACBrJsonObject;
  LJsonArray: TACBrJsonArray;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    // Adiciona um objeto
    LJsonObject := TACBrJsonObject.Create;
    LJsonObject.AddPair('subCampo', 'subValor');
    FJsonObject.AddPair('campoObjeto', LJsonArray);

    // Adiciona e destrói um array
    LJsonArray := TACBrJsonArray.Create;
    LJsonArray.AddElement('Item1');
    LJsonArray.AddElement('Item2');
    FJsonObject.AddPair('campoArray', LJsonArray);
    LJsonArray.Free; // Destroi o array

    // Verifica os valores
    CheckEquals('subValor', FJsonObject.AsJSONObject['campoObjeto'].AsString['subCampo'], 'Campo do objeto não foi adicionado corretamente');
    CheckEquals('Item1', FJsonObject.AsJSONArray['campoArray'].ItemAsJSONObject[0].ToString, 'Primeiro item do array não foi adicionado corretamente');
    CheckEquals('Item2', FJsonObject.AsJSONArray['campoArray'].ItemAsJSONObject[1].ToString, 'Segundo item do array não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestCheckNil;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('item1','teste');
    FJsonObject.AddPair('item2',123);
    CheckTrue(FJsonObject.AsJSONObject['itemObjeto'] = nil, 'O campo "itemObjeto" deveria ser nil');
    CheckTrue(FJsonObject.AsJSONArray['itemArray'] = nil, 'O campo "itemArray" deveria ser nil');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestValueExists;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('chave', 'valor');
    CheckTrue(FJsonObject.ValueExists('chave'), 'Valor não existe no objeto JSON');
    CheckFalse(FJsonObject.ValueExists('outraChave'), 'Valor não deveria existir no objeto JSON');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestIntegerValue;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('inteiro', 123);
    CheckEquals(123, FJsonObject.AsInteger['inteiro'], 'Valor inteiro não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestCurrencyValue;
var LValor : Currency;
begin
  LValor := 1234.4567555555;
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('currency', LValor);
    CheckEquals(1234.4567555555, FJsonObject.AsFloat['currency'], 0.0001, 'Valor float não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestFloatValue;
var LValor : Double;
begin
  LValor := 1234.4567555555;
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('float', LValor);
    CheckEquals(1234.4567555555, FJsonObject.AsFloat['float'], 0.0000000001, 'Valor float não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestArithmeticOperations;
var
  LResult: Double;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('valor1', 100.5);
    FJsonObject.AddPair('valor2', 200.75);
    LResult := FJsonObject.AsFloat['valor1'] + FJsonObject.AsFloat['valor2'];
    CheckEquals(301.25, LResult, 0.0001, 'Operação aritmética falhou');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestBooleanValue;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('booleano', True);
    CheckTrue(FJsonObject.AsBoolean['booleano'], 'Valor booleano não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestDateValue;
var
  LData: TDateTime;
begin
  LData := EncodeDate(2024, 7, 5);
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('data', DateToStr(LData));
    CheckEquals(DateToStr(LData), FJsonObject.AsString['data'], 'Valor de data não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestDateTimeValue;
var
  LDataHora: TDateTime;
begin
  LDataHora := EncodeDate(2024, 7, 5) + EncodeTime(14, 30, 0, 0);
  FJsonObject := TACBrJsonObject.Create;
  try
    FJsonObject.AddPair('dataHora', DateTimeToStr(LDataHora));
    CheckEquals(DateTimeToStr(LDataHora), FJsonObject.AsString['dataHora'], 'Valor de data e hora não foi adicionado corretamente');
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestErrorHandling;
begin
  FJsonObject := TACBrJsonObject.Create;
  try
    try
      FJsonObject.AddPair('chave', 123);
      FJsonObject.AsString['chave'];
      Fail('Acesso inválido a valor não gerou exceção');
    except
      on E: Exception do
        CheckTrue(True, 'Exceção capturada corretamente');
    end;
  finally
    FJsonObject.Free;
  end;
end;

procedure TTestACBrJson.TestReadAccentedWordFileUTF8;
var
  LFile : TStringList;
  LACBrObject : TACBrJsonObject;
  LACBrArray : TACBrJSONArray;
  I : Integer;

begin
  TestArquivo('..\..\Recursos\Json\UTF8-Dados1.json', '[UTF-8] A Leitura da Palavra Acentuada não foi lida corretamente');
end;

procedure TTestACBrJson.TestReadAccentedWordFileANSI;
var
  LFile : TStringList;
  LACBrObject : TACBrJsonObject;
  LACBrArray : TACBrJSONArray;
  I : Integer;
begin
  TestArquivo('..\..\Recursos\Json\ANSI-Dados1.json', '[ANSI] A Leitura da Palavra Acentuada não foi lida corretamente');
end;

procedure TTestACBrJson.TestGenerateAccentedWord;
var
  LACBrObject : TACBrJsonObject;
  LMinhaString : string;
begin
  try
    LACBrObject := TACBrJsonObject.Create;
    LACBrObject.AddPair('minhaString',WORD_ACCENTED);

    LMinhaString := '{''minhaString'':%s}';

    CheckNotEquals(WORD_ACCENTED,
                   Format(LMinhaString, [LACBrObject.ToJSON]),
                   'O Json Gerado é diferente do Esperado');
  finally
    LACBrObject.Free;
  end;
end;

initialization
  RegisterTest(TTestACBrJson);

end.

