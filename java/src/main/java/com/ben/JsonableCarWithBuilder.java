package com.ben;

import lombok.Builder;
import lombok.Data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Data
@Builder
@JsonInclude(Include.NON_NULL)
public class JsonableCarWithBuilder {
    
    @JsonProperty("make")
    private final String make;
    
    @JsonProperty("model")
    private final String model;
    
    @JsonProperty("year")
    private final Integer year;
    
    @JsonProperty("owner")
    private final Owner owner;
    
    public static void main(String[] args) {

        Owner owner = Owner
                .builder()
                .age(35)
                .name("Jonesy")
                .irritationFactor(155)
                .build();

        Owner owner2 = Owner
                .builder()
                .name("Frankerson")
                .build();
        
        JsonableCarWithBuilder car = JsonableCarWithBuilder
                .builder()
                .make("Chevrolet")
                .model("Silverado")
                .year(2004)
                .owner(owner)
                .build();
        
        JsonableCarWithBuilder car2 = JsonableCarWithBuilder
                .builder()
                .make("Ford")
                .model("F-250")
                .year(1999)
                .build();
        
        JsonableCarWithBuilder car3 = JsonableCarWithBuilder
                .builder()
                .make("Ford")
                .model("F-250")
                .owner(owner2)
                .build();
        
        System.out.println(car.getMake() + " " + car.getModel() + " " + car.getYear());
        System.out.println(car3.getMake() + " " + car3.getModel() + " " + car3.getYear());
        
        ObjectMapper objMapper = new ObjectMapper();
        String jsonString;
        
        try {
            jsonString = objMapper.writeValueAsString(car);
            System.out.println(jsonString);
            
            jsonString = objMapper.writeValueAsString(car3);
            System.out.println(jsonString);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
    }
}
