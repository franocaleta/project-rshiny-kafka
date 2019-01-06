from confluent_kafka.avro import AvroConsumer
import phonenumbers
from phonenumbers.phonenumberutil import region_code_for_number
import pandas as pd
import pycountry
c = AvroConsumer({'bootstrap.servers':'10.254.34.155:9092','group.id':'test-grupa','schema.registry.url':'http://10.254.34.155:8081','default.topic.config': {'auto.offset.reset': 'smallest'}})
c.subscribe(['tel_buildset_fer'])

def fun():
    return 1
def fun2():

    while True:
        try:
            first_msg = c.poll(10)
            mapa  = first_msg.value()
            print(mapa)
            mapa['CODE'] = region_code_for_number(phonenumbers.parse('+' + str(int(mapa['CALLEE']))))
            callee_country = pycountry.countries.get(alpha_2=region_code_for_number(phonenumbers.parse('+' + str(int(mapa['CALLEE'])))))
            mapa['CALLEE_COUNTRY']  = callee_country.name
            caller_country = pycountry.countries.get(alpha_2=region_code_for_number(phonenumbers.parse('+' + str(int(mapa['CALLER'])))))
            mapa['CALLER_COUNTRY']  = caller_country.name


            values = [list(mapa.values())] #spremamo vrijednosti prve poruke
            columns = list(mapa.keys()) #spremamo atribute prve poruke
            df = pd.DataFrame(values,columns=columns)
            return df
        except:
            continue
    #df.to_csv("dataset.csv")